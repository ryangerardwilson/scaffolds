open Cohttp
open Cohttp_lwt_unix
open Sqlite3



(* 
    Global session storage means than for x number users, x session 
    storage objects will be stored in memory at any given point of time 
    if they login together

    Each active user session corresponds to an object stored in memory. 
    So, for `x` users logged in simultaneously, there will be `x` 
    session objects in memory. For apps with less than 100,000 users, 
    this approach is often feasible, provided:

    - **Efficient Use of Data**: Session objects contain only necessary 
        information to avoid excessive memory usage.
    - **Memory Management**: Adequate system resources and monitoring 
        help handle peak loads and avoid memory exhaustion.
    - **Session Expiry**: Implementing session timeouts or expiration 
        to automatically free up memory for inactive sessions.
*)
let session_store : (string, string) Hashtbl.t = Hashtbl.create 16


(* Logs the user in *)
let handle_auth (body_str : string) =

  (* 1. Helper to open the DB using errcode. *)
  let db_open_local (path : string) =
    (*log "Attempting to open database at path: %s" path;*)
    let db = db_open path in
    match errcode db with
    | Rc.OK ->
        (*log "Database opened successfully.";*)
        db
    | rc ->
        (*log "Failed to open database with error: %s" (Rc.to_string rc);*)
        failwith ("Could not open DB: " ^ Rc.to_string rc)
  in

  (* 2. Parse "key=value" pairs from x-www-form-urlencoded. *)
  let decode_url str =
    let hex_to_char h = Char.chr (int_of_string ("0x" ^ h)) in
    let len = String.length str in
    let buf = Buffer.create len in
    let rec decode i =
      if i >= len then
        Buffer.contents buf
      else
        match str.[i] with
        | '%' when i + 2 < len ->
            Buffer.add_char buf (hex_to_char (String.sub str (i + 1) 2));
            decode (i + 3)
        | '+' ->
            Buffer.add_char buf ' ';
            decode (i + 1)
        | c ->
            Buffer.add_char buf c;
            decode (i + 1)
    in
    decode 0
  in

  (* 3. Function to parse "key=value" pairs from x-www-form-urlencoded string *)
  let parse_post_body form_body =
    (*log "Parsing form body: %s" form_body;*)
    let parts = Str.split (Str.regexp_string "&") form_body in
    List.map (fun part ->
      match Str.bounded_split (Str.regexp_string "=") part 2 with
      | [k; v] ->
          let decoded_key = decode_url k in
          let decoded_value = decode_url v in
          (decoded_key, decoded_value)
      | _ -> ("", "")
    ) parts
  in

  (* 4. A helper to create a random session. *)
  let create_session (username : string) : string =
    (*log "Creating session for user: %s" username;*)
    let rand_bytes = Bytes.create 16 in
    for i = 0 to 15 do
      Bytes.set rand_bytes i (char_of_int (Random.int 256))
    done;
    let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
    Hashtbl.replace session_store session_id username;
    (*log "Session created with ID: %s" session_id;*)
    session_id
  in

  (* 5. Helper: convert Data.t to string option. *)
  let data_to_string_opt (d : Data.t) : string option =
    match d with
    | Data.NULL -> None
    | Data.TEXT txt -> Some txt
    | _ -> None
  in

  (* 6. Open the database. *)
  let db_path = "dbs/auth/auth.db" in
  let db = db_open_local db_path in

  (* 7. Parse the form data. Keep them as string option. *)
  let form_data = parse_post_body body_str in
  let email_submitted = List.assoc_opt "email" form_data in
  let password_submitted = List.assoc_opt "password" form_data in

  (* 8. Prepare and bind for the query. *)
  let stmt = prepare db "SELECT email, password FROM users WHERE email = ?" in
  (match email_submitted with
   | Some e ->
       ignore (bind_text stmt 1 e)
   | None ->
       ignore (bind stmt 1 Data.NULL));

  (* 9. maybe_user is a string option: Some db_email if the password matches, None otherwise. *)
  let maybe_user =
    match step stmt with
    | Rc.ROW ->
        let db_email = data_to_string_opt (column stmt 0) in
        let db_password = data_to_string_opt (column stmt 1) in
        if db_password = password_submitted then
          db_email
        else
          None
    | Rc.DONE -> None
    | rc -> failwith ("Error stepping statement: " ^ Rc.to_string rc)
  in

  ignore (finalize stmt);
  ignore (db_close db);

  (* 10. If maybe_user is Some e, create a session; otherwise return an error. *)
  match maybe_user with
  | Some user_email ->
      let session_id = create_session user_email in
      let headers =
        Cohttp.Header.init ()
        |> fun h -> Cohttp.Header.add h "Set-Cookie"
          ("sessionid=" ^ session_id ^ "; Path=/")
      in
      Ok (headers, Uri.of_string "/dashboard")
  | None ->
      Error "Invalid credentials. Please try again."




(* Determine if a user is logged in by checking the session *)
let get_username_if_user_is_logged_in req =
  (* Function to extract session ID from cookie string; returns a string option *)
  let get_session_id_from_cookie cookie_str =
    let parts = String.split_on_char ';' cookie_str in
    let find_sessionid kv =
      let kv = String.trim kv in
      if String.length kv >= 10 && String.sub kv 0 10 = "sessionid="
      then Some (String.sub kv 10 (String.length kv - 10))
      else None
    in
    List.fold_left
      (fun acc item -> match acc with None -> find_sessionid item | Some _ -> acc)
      None
      parts
  in

  (* Function to retrieve a username associated with a session ID; returns a string option *)
  let get_username_for_session session_id =
    Hashtbl.find_opt session_store session_id
  in

  let cookie_header = Cohttp.Header.get (Request.headers req) "cookie" in
  match cookie_header with
  | None -> None
  | Some cookie_str ->
      (* Instead of Option.bind, just manually match on the option. *)
      match get_session_id_from_cookie cookie_str with
      | None -> None
      | Some session_id -> get_username_for_session session_id


(*
  Given a cookie string (e.g., "sessionid=xyz; Path=/"), extract
  the value of sessionid. 
*)
let handle_cookie (req : Request.t) : string option =
  (* Inline function to extract session ID from a cookie string *)
  let extract_session_id cookie_str =
    let parts = String.split_on_char ';' cookie_str in
    let rec find_sessionid = function
      | [] -> None
      | kv :: rest ->
        let kv = String.trim kv in
        if String.length kv >= 10 && String.sub kv 0 10 = "sessionid="
        then Some (String.sub kv 10 (String.length kv - 10))
        else find_sessionid rest
    in
    find_sessionid parts
  in

  (* Inline function to look up username for a given session ID *)
  let get_username_for_session session_id =
    Hashtbl.find_opt session_store session_id
  in

  let headers = Request.headers req in
  match Header.get headers "cookie" with
  | None -> None
  | Some cookie_str ->
    match extract_session_id cookie_str with
    | None -> None
    | Some sid ->
      get_username_for_session sid






(* Removes an existing session from the store by session ID in the request. *)
let handle_session_destruction (req : Cohttp.Request.t) : unit =
  let extract_session_id (cookie_str : string) : string option =
    let parts = String.split_on_char ';' cookie_str in
    let find_sessionid kv =
      let kv = String.trim kv in
      if String.length kv >= 10 && String.sub kv 0 10 = "sessionid="
      then Some (String.sub kv 10 (String.length kv - 10))
      else None
    in
    List.fold_left
      (fun acc item ->
         match acc with
         | None -> find_sessionid item
         | Some _ -> acc)
      None
      parts
  in

  let cookie_header = Cohttp.Header.get (Request.headers req) "cookie" in
  match cookie_header with
  | None -> ()
  | Some cookie_str ->
      match extract_session_id cookie_str with
      | Some session_id -> Hashtbl.remove session_store session_id
      | None -> ()

