open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

let handle_dashboard _conn req _body =
  match Renderer.handle_cookie req with
  | None ->
      Server.respond_string ~status:`Forbidden
        ~body:"No valid session cookie or session expired. \
               Please <a href=\"/login\">log in</a>."
        ()
  | Some username ->
      let filename = "dashboard.html" in
      let substitutions = [("{{USERNAME}}", username)] in
      Renderer.server_side_render filename substitutions

