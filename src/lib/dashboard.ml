open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Debugger  (* So we can call dump_and_die. *)

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
      (* Immediately “dump and die,” short-circuiting this request. 
         The rest of this function won't be reached. *)
      (* 
      dump_and_die "Something went wrong (or you just want to see data)!"
      >>= fun _ ->
      *) 
      (* The code below is never reached after dump_and_die. 
         Shown only for clarity if you removed the dump_and_die call. *)
      let filename = "dashboard.html" in
      let substitutions = [
        ("{{APP_NAME}}", app_name);
        ("{{USERNAME}}", username_string)
      ] in
      (*
      dump_and_die [any filename; any substitutions]
      >>= fun _ ->
      *)
      Renderer.server_side_render filename substitutions

