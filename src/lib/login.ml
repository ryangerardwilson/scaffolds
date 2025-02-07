open Cohttp_lwt_unix
open Lwt.Infix

let handle_login _conn req body =
  let app_name = Sys.getenv "APP_NAME" in

  match Request.meth req with
  | `GET ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let filename = "login.html" in
      let input_list = [Debugger.any req] in
      let output_list = [Debugger.any filename] in
      Lwt.async (fun () -> Debugger.log_event "/login => handle_login => GET" input_list output_list);
      Server.respond_string ~status:`OK ~body:body_str ()
  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let auth_result = Authentication.handle_auth body_str in
      begin match auth_result with
      | Ok (headers, uri) ->
          let input_list = [Debugger.any req; Debugger.any body_str] in
          let output_list = [Debugger.any (Printf.sprintf "Redirecting to %s" (Uri.to_string uri))] in
          Lwt.async (fun () -> Debugger.log_event "/login => handle_login => POST" input_list output_list);
          Server.respond_redirect ~headers ~uri ()
      | Error error_msg ->
          let filename = "login.html" in
          let substitutions = [
            ("{{APP_NAME}}", app_name);
            ("{{ERROR_MESSAGE}}", error_msg)
          ] in
          let input_list = [Debugger.any req] in
          let output_list = [Debugger.any filename; Debugger.any substitutions] in
          Lwt.async (fun () -> Debugger.log_event "/login => handle_login => POST => Error" input_list output_list);
          Renderer.server_side_render filename substitutions
      end

  | _ ->
      Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()

