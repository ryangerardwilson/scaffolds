open Cohttp_lwt_unix

let handle_logout _conn req _body =
  (* Destroy the session associated with the request *)
  Authentication.handle_session_destruction req;
  (* Prepare output string for logging *)
  let output = "Redirecting to /" in
  (* Prepare the logging input and output lists *)
  let input_list = [Debugger.any req] in
  let output_list = [Debugger.any output] in
  Lwt.async (fun () -> Debugger.log_event "/logout => handle_logout" input_list output_list);
  (* Redirect to landing page ("/") *)
  Server.respond_redirect ~uri:(Uri.of_string "/") ()

