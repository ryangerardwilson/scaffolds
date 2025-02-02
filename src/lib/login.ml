open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix


let handle_login _conn req body =
  match Request.meth req with
  | `GET ->
      Renderer.server_side_render "login.html" []
  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Renderer.handle_auth body_str
  | _ ->
      Server.respond_string ~status:`Method_not_allowed
        ~body:"Method not allowed"
        ()

