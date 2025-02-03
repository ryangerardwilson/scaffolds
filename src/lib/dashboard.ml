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

