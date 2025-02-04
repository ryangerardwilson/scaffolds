open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

(* Handler for /logout *)
let handle_logout _conn req _body =
  (* Destroy the session associated with the request *)
  Renderer.handle_session_destruction req;
  (* Redirect them to landing page ("/") or wherever you prefer *)
  Server.respond_redirect ~uri:(Uri.of_string "/") ()

