open Lwt.Infix
open Cohttp_lwt_unix

let () = Dotenv.export ()
let () = Random.self_init ()

(* List of routes for which logging is enabled *)
let logged_routes = ["/"; "/about"; "/signup"; "/login"; "/logout"; "/dashboard"]

(* Your existing routes *)
let routes = [
  ("/", Landing.handle_landing);
  ("/about", About.handle_about);
  ("/signup", Signup.handle_signup);
  ("/login", Login.handle_login);
  ("/logout", Logout.handle_logout);
  ("/dashboard", Dashboard.handle_dashboard);
]

(* Wrap everything in Lwt.catch to intercept DumpAndDie exceptions. *)
let route conn req body =
  Lwt.catch
    (fun () ->
       let uri_path = Uri.path (Request.uri req) in
       if Astring.String.is_prefix ~affix:"/assets" uri_path then
         Assets.handle_assets conn req body
       else
         match List.find_opt (fun (p, _) -> p = uri_path) routes with
         | Some (_, handler) ->
             (* For endpoints with logging enabled, capture the request body, then call handler and log the output. *)
             if List.mem uri_path logged_routes then
               Cohttp_lwt.Body.to_string body >>= fun req_body ->
               let new_body = Cohttp_lwt.Body.of_string req_body in
               handler conn req new_body >>= fun (resp, resp_body) ->
               let status = Cohttp.Response.status resp in
               let resp_summary = Printf.sprintf "HTTP %s" (Cohttp.Code.string_of_status status) in
               (* Using intermediate variables to disambiguate the list literal *)
               let req_list = [Debugger.any req_body] in
               let out_list = [Debugger.any resp_summary] in
               Lwt.async (fun () -> Debugger.log_event uri_path req_list out_list);
               Lwt.return (resp, resp_body)
             else
               handler conn req body
         | None ->
             Server.respond_string ~status:`Not_found ~body:"Not Found" ()
    )
    (function
      | Debugger.DumpAndDie debug_msg ->
          Server.respond_string ~status:`OK ~body:debug_msg ()
      | ex ->
          Server.respond_string ~status:`Internal_server_error
            ~body:("Unhandled exception: " ^ Printexc.to_string ex) ()
    )

(* Retrieve environment variables. *)
let app_name = Sys.getenv "APP_NAME"
let port = int_of_string (Sys.getenv "PORT")

(* Main entry point. *)
let () =
  Lwt_main.run begin
    Migrations.initiate_migrations () >>= function
    | false ->
        Printf.eprintf "[ERROR] Failed to run migrations, not starting server.\n%!";
        Lwt.return_unit
    | true ->
        Printf.printf "[INFO] Running %s on port %d\n%!" app_name port;
        let config = Server.make ~callback:route () in
        Server.create ~mode:(`TCP (`Port port)) config
  end

