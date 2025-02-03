(* AUTO-GENERATED by compile.py Step 0 *)

let file_main_ext_ml = {|

open Lwt.Infix
open Cohttp_lwt_unix

let routes = [
  ("/", Landing.handle_landing);
  ("/about", About.handle_about);
  ("/login", Login.handle_login);
  ("/logout", Logout.handle_logout);
  ("/dashboard", Dashboard.handle_dashboard);
]

let route conn req body =
  let uri_path = Uri.path (Request.uri req) in
  match List.find_opt (fun (p, _) -> p = uri_path) routes with
  | Some (_, handler) -> handler conn req body
  | None ->
      Server.respond_string ~status:`Not_found ~body:"Not Found" ()


let app_name = Sys.getenv "APP_NAME"
let port = int_of_string (Sys.getenv "PORT")

(* Ensure Random is seeded once *)
let () = Random.self_init ()

let () =
  Printf.printf "Starting %s on port %d\n%!" app_name port;
  let config = Server.make ~callback:route () in
  Lwt_main.run (Server.create ~mode:(`TCP (`Port port)) config)

|}

let ext_env = {|

APP_NAME=YourApp
PUBLIC_DIR=resources
PORT=8080

|}

let file_compile_ext_sh = {|
#!/bin/bash

# Step 1: Compile modules
ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib utils/renderer.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/landing.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/about.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/login.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/logout.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/dashboard.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib main.ml

# Step 2: Link modules
ocamlfind ocamlc -thread -package cohttp-lwt-unix,dotenv,str,base64 -linkpkg \
  -o app \
  utils/renderer.cmo \
  lib/landing.cmo \
  lib/about.cmo \
  lib/login.cmo \
  lib/logout.cmo \
  lib/dashboard.cmo \
  main.cmo

# Step 3: Clean .cmi, .cmo, .out
find . -type f \( -name "*.cmo" -o -name "*.cmi" -o -name "*.out" \) -exec rm -f {} +

echo "Use the --and_run flag to compile and run the app automatically."

# Step 4: Optionally run
if [[ "$1" == "--and_run" ]]; then
  ./app
fi


|}

let dir_utils_file_renderer_ext_ml = {|
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

(*
    This line of code exports the env file in memory
*)
let () = Dotenv.export ()

let get_env var =
  try
    Sys.getenv var
  with
  | Not_found -> "None"

(* Generates a random session_id, then associates it with the given username. *)
let create_session ~(username : string) : string =
  let rand_bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set rand_bytes i (char_of_int (Random.int 256))
  done;
  let session_id = Base64.encode_exn (Bytes.to_string rand_bytes) in
  Hashtbl.replace session_store session_id username;
  session_id

(* Removes an existing session from the store by session ID in the request. *)
let destroy_session (req : Cohttp.Request.t) : unit =
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


|}

let dir_resources_file_about_ext_html = {|
<html>
  <head>
    <title>{{APP_NAME}}/ {{PAGE_TITLE}}</title>
  </head>
  <body>
    <h2>About Page</h1>
    
    <!-- We'll inject either "logged in as..." or nothing here: -->
    <div>{{LOGGED_IN_AS}}</div>

    <!-- We'll also inject the link block (login or dashboard/logout) here: -->
    <div>{{LINK_BLOCK}}</div>

    <div>{{ABOUT_CONTENT}}</div>
  </body>
</html>


|}

let dir_resources_file_login_ext_html = {|
<html>
  <head>
    <title>{{APP_NAME}} / Login</title>
  </head>
  <body>
    <h2>Login</h2>
    {{ERROR_MESSAGE}}
    <form method="POST" action="/login">
      <label>Username:
        <input type="text" name="username"/>
      </label>
      <br/>
      <label>Password:
        <input type="password" name="password"/>
      </label>
      <br/>
      <input type="submit" value="Login"/>
    </form>
  </body>
</html>


|}

let dir_resources_file_landing_ext_html = {|

<!-- dist/landing.html -->
<html>
  <head>
    <title>{{APP_NAME}}/ Landing Page</title>
  </head>
  <body>
    <h1>Welcome to Our Simple OCaml App</h1>

    <!-- We'll inject either "logged in as..." or nothing here: -->
    <div>{{LOGGED_IN_AS}}</div>

    <!-- We'll also inject the link block (login or dashboard/logout) here: -->
    <div>{{LINK_BLOCK}}</div>
  </body>
</html>

|}

let dir_resources_file_dashboard_ext_html = {|

<html>
  <head>
    <title>{{APP_NAME}}/ Dashboard</title>
  </head>
  <body>
    <h1>Dashboard</h1>
    <p>Welcome, {{USERNAME}}!</p>
    <p><a href="/">Go back to Landing Page</a></p>
  </body>
</html>

|}

let dir_lib_file_login_ext_ml = {|
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix


let handle_login _conn req body =
  let app_name = Sys.getenv "APP_NAME" in
  let substitutions = [
    ("{{APP_NAME}}", app_name);
    ("{{ERROR_MESSAGE}}", ""); 
  ] in

  match Request.meth req with
  | `GET ->
      Renderer.server_side_render "login.html" substitutions >>= fun (response, body) ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Server.respond_string ~status:`OK ~body:body_str ()

  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let auth_result = Renderer.handle_auth body_str in
      begin match auth_result with
      | Ok (headers, uri) ->
          Server.respond_redirect ~headers ~uri ()
      | Error error_msg ->
          let substitutions = [
            ("{{APP_NAME}}", app_name);
            ("{{ERROR_MESSAGE}}", error_msg); (* Default to no error message *)
          ] in


          let error_substitutions = substitutions @ [("{{ERROR_MESSAGE}}", "<p style='color:red;'>" ^ error_msg ^ "</p>")] in
          Renderer.server_side_render "login.html" error_substitutions >>= fun (response, body) ->
          Cohttp_lwt.Body.to_string body >>= fun body_str ->
          Server.respond_string ~status:`OK ~body:body_str ()
      end

  | _ ->
      Server.respond_string ~status:`Method_not_allowed
        ~body:"Method not allowed" ()


|}

let dir_lib_file_logout_ext_ml = {|
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

(* Handler for /logout *)
let handle_logout _conn req _body =
  (* Destroy the session associated with the request *)
  Renderer.destroy_session req;
  (* Redirect them to landing page ("/") or wherever you prefer *)
  Server.respond_redirect ~uri:(Uri.of_string "/") ()


|}

let dir_lib_file_about_ext_ml = {|
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix


(* Handle landing page requests *)
let handle_about _conn req _body =
  let username = Renderer.get_username_if_user_is_logged_in req in

  let logged_in_as_html =
    match username with
    | Some username_string -> Printf.sprintf "Logged in as %s" username_string
    | None -> ""
  in

  let link_block_html =
    match username with
    | Some _ ->
      "<p><a href=\"/dashboard\">Go to Dashboard</a> | <a href=\"/logout\">Logout</a></p>"
    | None ->
      "<p><a href=\"/login\">Login</a> | <a href=\"/landing\">Landing</a></p>"
  in

  let app_name = Sys.getenv "APP_NAME" in

  let substitutions = [
    ("{{APP_NAME}}",app_name);
    ("{{PAGE_TITLE}}", "About Page 777");
    ("{{ABOUT_CONTENT}}", "This is the about page content. 771");
    ("{{LOGGED_IN_AS}}", logged_in_as_html);
    ("{{LINK_BLOCK}}", link_block_html);
  ] in

  Renderer.server_side_render "about.html" substitutions


|}

let dir_lib_file_dashboard_ext_ml = {|
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

let handle_dashboard _conn req _body =
  let username = Renderer.get_username_if_user_is_logged_in req in

  let app_name = Sys.getenv "APP_NAME" in

  match username with
  | None ->
      Server.respond_string ~status:`Forbidden
        ~body:"No valid session or not logged in. \
               Please <a href=\"/login\">log in</a>."
        ()
  | Some username_string ->
      let filename = "dashboard.html" in
      let substitutions = [
        ("{{APP_NAME}}", app_name);
        ("{{USERNAME}}", username_string)
      ] in
      Renderer.server_side_render filename substitutions


|}

let dir_lib_file_landing_ext_ml = {|
(* lib/Landing.ml *)

open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Base64  (* Assume Base64 is used for encoding *)


(* Handle landing page requests *)
let handle_landing _conn req _body =
  let username = Renderer.get_username_if_user_is_logged_in req in

  let logged_in_as_html =
    match username with
    | Some username_string -> Printf.sprintf "Logged in as %s" username_string
    | None -> ""
  in

  let link_block_html =
    match username with
    | Some _ ->
      "<p><a href=\"/dashboard\">Go to Dashboard</a> | <a href=\"/about\">About</a> | <a href=\"/logout\">Logout</a></p>"
    | None ->
      "<p><a href=\"/login\">Login</a> | <a href=\"/about\">About</a></p>"
  in

  let app_name = Sys.getenv "APP_NAME" in

  let substitutions = [
    ("{{APP_NAME}}", app_name);
    ("{{LOGGED_IN_AS}}", logged_in_as_html);
    ("{{LINK_BLOCK}}", link_block_html);
  ] in

  Renderer.server_side_render "landing.html" substitutions


|}

