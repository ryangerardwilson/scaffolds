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
      "<p><a href=\"/login\">Login</a> | <a href=\"/\">Landing</a></p>"
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

