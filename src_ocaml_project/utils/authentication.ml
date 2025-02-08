open Cohttp
open Cohttp_lwt_unix

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

(* Handles the signup logic.
   Expects POST body with the fields "email", "password", and "confirm_password". *)
let handle_signup (body_str : string) : (Cohttp.Header.t * Uri.t, string) result =

  (* Helper: decode percent-encoded URL strings. *)
  let decode_url str =
    let hex_to_char h = Char.chr (int_of_string ("0x" ^ h)) in
    let len = String.length str in
    let buf = Buffer.create len in
    let rec decode i =
      if i >= len then Buffer.contents buf else
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

  (* Helper: parse "key=value" pairs from an x-www-form-urlencoded string *)
  let parse_post_body form_body =
    let parts = Str.split (Str.regexp_string "&") form_body in
    List.map (fun part ->
      match Str.bounded_split (Str.regexp_string "=") part 2 with
      | [k; v] -> (decode_url k, decode_url v)
      | _ -> ("", "")
    ) parts
  in

  (* Helper: create a random session for a user *)
  let create_session username =
    let rand_bytes = Bytes.create 16 in
    for i = 0 to 15 do
      Bytes.set rand_bytes i (char_of_int (Random.int 256))
    done;
    let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
    Hashtbl.replace session_store session_id username;
    session_id
  in

  let form_data = parse_post_body body_str in
  let email_opt = List.assoc_opt "email" form_data in
  let password_opt = List.assoc_opt "password" form_data in
  let confirm_opt = List.assoc_opt "confirm_password" form_data in
  
  match (email_opt, password_opt, confirm_opt) with
  | (Some email, Some password, Some confirm_password) ->
      if password <> confirm_password then
        Result.Error "Passwords do not match."
      else
        (* First check if the user already exists. Using basic mode for simplicity *)
        let check_result = 
          Database.query_to_df "auth" "users" (Printf.sprintf "SELECT email FROM users WHERE email = '%s'" email) "basic"
        in
        let exists =
          match check_result with
          | Basic_df bdf -> bdf.rows <> []
          | Owl_df odf -> Owl_dataframe.shape odf |> fun (m, _) -> m > 0
        in
        if exists then
          Result.Error "User already exists."
        else (
          (* Insert the new user via the new insert method.
             Details must be provided as a list of (column, value) pairs. *)
          Database.insert "auth" "users" [("email", email); ("password", password)];
          
          (* Create a session and set a redirect header *)
          let session_id = create_session email in
          let headers = Cohttp.Header.add (Cohttp.Header.init ()) "Set-Cookie" ("sessionid=" ^ session_id ^ "; Path=/") in
          Result.Ok (headers, Uri.of_string "/dashboard")
        )
  | _ ->
      Result.Error "Missing required signup fields."


(* Logs the user in *)
let handle_auth (body_str : string) : (Cohttp.Header.t * Uri.t, string) result =
  (* Helper: decode percent-encoded URL strings *)
  let decode_url str =
    let hex_to_char h = Char.chr (int_of_string ("0x" ^ h)) in
    let len = String.length str in
    let buf = Buffer.create len in
    let rec decode i =
      if i >= len then Buffer.contents buf
      else match str.[i] with
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

  (* Helper: parse x-www-form-urlencoded key=value pairs *)
  let parse_post_body form_body =
    let parts = Str.split (Str.regexp_string "&") form_body in
    List.map (fun part ->
      match Str.bounded_split (Str.regexp_string "=") part 2 with
      | [k; v] -> (decode_url k, decode_url v)
      | _ -> ("", "")
    ) parts
  in

  (* Helper: create a random session for a user *)
  let create_session username =
    let rand_bytes = Bytes.create 16 in
    for i = 0 to 15 do
      Bytes.set rand_bytes i (char_of_int (Random.int 256))
    done;
    let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
    Hashtbl.replace session_store session_id username;
    session_id
  in

  (* Helper: Convert a basic Database.cell to a string option *)
  let cell_to_text_opt cell =
    match cell with
    | Database.Text s -> Some s
    | _ -> None
  in

  (* Parse the POST body *)
  let form_data = parse_post_body body_str in
  let email_submitted = List.assoc_opt "email" form_data in
  let password_submitted = List.assoc_opt "password" form_data in

  match (email_submitted, password_submitted) with
  | (Some email, Some password) ->
      (* Build the SQL query. (In production consider parameter binding to avoid injection) *)
      let sql = Printf.sprintf "SELECT email, password FROM users WHERE email = '%s'" email in
      let query_result = Database.query_to_df "auth" "users" sql "basic" in
      let maybe_user =
        match query_result with
        | Database.Basic_df bdf ->
            (match bdf.rows with
             | [row] ->
                 (match row with
                  | [email_cell; password_cell] ->
                      let db_email_opt = cell_to_text_opt email_cell in
                      let db_password_opt = cell_to_text_opt password_cell in
                      if db_password_opt = Some password then db_email_opt else None
                  | _ -> None)
             | _ -> None)
        | _ -> None
      in
      (match maybe_user with
       | Some user_email ->
           let session_id = create_session user_email in
           let headers =
             Cohttp.Header.add (Cohttp.Header.init ())
               "Set-Cookie" ("sessionid=" ^ session_id ^ "; Path=/")
           in
           Ok (headers, Uri.of_string "/dashboard")
       | None ->
           Error "Invalid credentials. Please try again.")
  | _ ->
      Error "Missing credentials."

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




