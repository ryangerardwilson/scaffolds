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

