open Cohttp_lwt_unix
open Lwt.Infix



(*
  Renders an HTML file from disk (defaults to the "dist" directory),
  performing string substitutions like [("{{USERNAME}}", "Alice"); ...].
*)
let server_side_render (filename : string) 
                       (substitutions : (string * string) list)
                       : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
  let public_dir = Sys.getenv_opt "PUBLIC_DIR" |> Option.value ~default:"dist" in
  let filepath = Filename.concat public_dir filename in
  if Sys.file_exists filepath then
    Lwt_io.(with_file ~mode:Input filepath read) >>= fun content ->
    let replaced_content =
      List.fold_left
        (fun acc (needle, replacement) ->
           Str.global_replace (Str.regexp_string needle) replacement acc
        )
        content
        substitutions
    in
    Server.respond_string ~status:`OK ~body:replaced_content ()
  else
    Server.respond_string
      ~status:`Not_found
      ~body:"File not found"
      ()

