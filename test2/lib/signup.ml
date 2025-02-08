open Cohttp_lwt_unix
open Lwt.Infix

let handle_signup _conn req body =
  let app_name = Sys.getenv "APP_NAME" in
  let base_substitutions = [
    ("{{APP_NAME}}", app_name);
    ("{{ERROR_MESSAGE}}", "");
  ] in

  match Request.meth req with
  | `GET ->
      Renderer.server_side_render "signup.html" base_substitutions >>= fun (response, render_body) ->
      Cohttp_lwt.Body.to_string render_body >>= fun body_str ->
      let input_list = [Debugger.any req] in
      let output_list = [Debugger.any body_str] in
      Lwt.async (fun () -> Debugger.log_event "/signup => handle_signup => GET" input_list output_list);
      Server.respond_string ~status:`OK ~body:body_str ()

  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      (match Authentication.handle_signup body_str with
       | Result.Ok (headers, uri) ->
           let redirect_msg = Printf.sprintf "Redirecting to %s" (Uri.to_string uri) in
           let input_list = [Debugger.any req; Debugger.any body_str] in
           let output_list = [Debugger.any redirect_msg] in
           Lwt.async (fun () -> Debugger.log_event "/signup => handle_signup => POST" input_list output_list);
           Server.respond_redirect ~headers ~uri ()
       | Result.Error error_msg ->
           let error_substitutions = [
             ("{{APP_NAME}}", app_name);
             ("{{ERROR_MESSAGE}}", "<p style='color:red;'>" ^ error_msg ^ "</p>");
           ] in
           Renderer.server_side_render "signup.html" error_substitutions >>= fun (response, render_body) ->
           Cohttp_lwt.Body.to_string render_body >>= fun rendered_body ->
           let input_list = [Debugger.any req; Debugger.any body_str] in
           let output_list = [Debugger.any rendered_body] in
           Lwt.async (fun () -> Debugger.log_event "/signup => handle_signup => POST => Error" input_list output_list);
           Server.respond_string ~status:`OK ~body:rendered_body ()
      )
  | _ ->
      let input_list = [Debugger.any req] in
      let output_list = [Debugger.any "Method not allowed"] in
      Lwt.async (fun () -> Debugger.log_event "/signup => handle_signup => METHOD_NOT_ALLOWED" input_list output_list);
      Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()

