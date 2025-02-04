open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

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


let get_env var =
  try
    Sys.getenv var
  with
  | Not_found -> "None"

(* Generates a random session_id, then associates it with the given username. *)
(*
let create_session ~(username : string) : string =
  let rand_bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set rand_bytes i (char_of_int (Random.int 256))
  done;
  let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
  Hashtbl.replace session_store session_id username;
  session_id
*)

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


(* Logs the user in *)
let handle_auth body_str =
  (* Local function to create a session. *)
  let create_session ~(username : string) : string =
    let rand_bytes = Bytes.create 16 in
    for i = 0 to 15 do
      Bytes.set rand_bytes i (char_of_int (Random.int 256))
    done;
    let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
    Hashtbl.replace session_store session_id username;
    session_id
  in

  (* Define valid users as a list of tuples (username, password) *)
  let valid_users = [
    ("admin", "secret");
    ("bob", "bob123");
    ("alice", "alice123");
  ] in

  let parse_post_body (body_str : string) : (string * string) list =
    let parts = Str.split (Str.regexp_string "&") body_str in
    List.map (fun part ->
        match Str.bounded_split (Str.regexp_string "=") part 2 with
        | [k; v] -> (k, v)
        | _ -> ("", "")
      ) parts
  in

  let form_data = parse_post_body body_str in
  let username_submitted = List.assoc_opt "username" form_data |> Option.value ~default:"" in
  let password_submitted = List.assoc_opt "password" form_data |> Option.value ~default:"" in

  let maybe_user =
    List.find_opt
      (fun (username, password) -> username = username_submitted && password = password_submitted)
      valid_users
  in

  match maybe_user with
  | Some (username, _) ->
      let session_id = create_session ~username in
      let headers = Header.add (Header.init ()) "Set-Cookie"
          ("sessionid=" ^ session_id ^ "; Path=/") in
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


(*
  Renders an HTML file from disk (defaults to the "dist" directory),
  performing string substitutions like [("{{USERNAME}}", "Alice"); ...].
*)
let server_side_render (filename : string) 
                       (substitutions : (string * string) list)
                       : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
  let public_dir = Sys.getenv_opt "PUBLIC_DIR" |> Option.value ~default:"dist" in
  let filepath = Filename.concat public_dir filename in
  if Sys.file_exists filepath then
    Lwt_io.(with_file ~mode:Input filepath read) >>= fun content ->
    let replaced_content =
      List.fold_left
        (fun acc (needle, replacement) ->
           Str.global_replace (Str.regexp_string needle) replacement acc
        )
        content
        substitutions
    in
    Server.respond_string ~status:`OK ~body:replaced_content ()
  else
    Server.respond_string
      ~status:`Not_found
      ~body:"File not found"
      ()

