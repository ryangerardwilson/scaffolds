open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix


let handle_login _conn req body =
  let app_name = Sys.getenv "APP_NAME" in
  let substitutions = [
    ("{{APP_NAME}}", app_name);
    ("{{ERROR_MESSAGE}}", ""); 
  ] in

  match Request.meth req with
  | `GET ->
      Renderer.server_side_render "login.html" substitutions >>= fun (response, body) ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      Server.respond_string ~status:`OK ~body:body_str ()

  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let auth_result = Renderer.handle_auth body_str in
      begin match auth_result with
      | Ok (headers, uri) ->
          Server.respond_redirect ~headers ~uri ()
      | Error error_msg ->
          let substitutions = [
            ("{{APP_NAME}}", app_name);
            ("{{ERROR_MESSAGE}}", error_msg); (* Default to no error message *)
          ] in


          let error_substitutions = substitutions @ [("{{ERROR_MESSAGE}}", "<p style='color:red;'>" ^ error_msg ^ "</p>")] in
          Renderer.server_side_render "login.html" error_substitutions >>= fun (response, body) ->
          Cohttp_lwt.Body.to_string body >>= fun body_str ->
          Server.respond_string ~status:`OK ~body:body_str ()
      end

  | _ ->
      Server.respond_string ~status:`Method_not_allowed
        ~body:"Method not allowed" ()

