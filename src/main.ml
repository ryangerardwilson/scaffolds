(* File: main.ml *)

open Lwt.Infix
open Cohttp_lwt_unix

(* Reference your Assets module. *)
module Assets = Assets

(* Define other routes as before. *)
let routes = [
  ("/", Landing.handle_landing);
  ("/about", About.handle_about);
  ("/login", Login.handle_login);
  ("/logout", Logout.handle_logout);
  ("/dashboard", Dashboard.handle_dashboard);
]

(* Custom route function. Note we check "/assets" prefix first. *)
let route conn req body =
  let uri_path = Uri.path (Request.uri req) in

  if Astring.String.is_prefix ~affix:"/assets" uri_path then
    (* Dispatch to the Assets module *)
    Assets.handle_assets conn req body
  else
    (* Otherwise, see if we have an exact match in routes. *)
    match List.find_opt (fun (p, _) -> p = uri_path) routes with
    | Some (_, handler) ->
        handler conn req body
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

