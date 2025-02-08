open Cohttp_lwt_unix

let handle_dashboard _conn req _body =
  let username = Authentication.get_username_if_user_is_logged_in req in
  let app_name = Sys.getenv "APP_NAME" in

  match username with
  | None ->
      (* Log response *)
      let response_body =
        "No valid session or not logged in. Please <a href=\"/login\">log in</a>." in
      let input_list = [Debugger.any req] in
      let output_list = [Debugger.any response_body] in
      Lwt.async (fun () -> Debugger.log_event "/dashboard" input_list output_list);
      Server.respond_string ~status:`Forbidden ~body:response_body ()
  | Some username_string ->
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
      (* Log response *)
      let input_list = [Debugger.any req] in
      let output_list = [Debugger.any filename; Debugger.any substitutions] in
      Lwt.async (fun () -> Debugger.log_event "/dashboard => handle_dashboard" input_list output_list);
      Renderer.server_side_render filename substitutions


