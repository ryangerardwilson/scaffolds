open Cohttp_lwt_unix

(* Serve any static file under "PUBLIC_DIR/assets". *)
let handle_assets conn req _body =
  (* Figure out which directory to serve from (defaults to "dist" if PUBLIC_DIR not set). *)
  let public_dir =
    Sys.getenv_opt "PUBLIC_DIR"
    |> Option.value ~default:"dist"
  in

  (* The requested URI path, e.g. "/assets/styles.css". *)
  let uri_path = Uri.path (Request.uri req) in

  (* Strip the prefix "/assets" (and possibly the extra slash). *)
  let subpath =
    if Astring.String.is_prefix ~affix:"/assets/" uri_path then
      (* e.g. "/assets/styles.css" => subpath "styles.css" *)
      let offset = String.length "/assets/" in
      String.sub uri_path offset (String.length uri_path - offset)
    else if Astring.String.is_prefix ~affix:"/assets" uri_path then
      (* e.g. exactly "/assets" => subpath "" *)
      let offset = String.length "/assets" in
      if String.length uri_path > offset then
        String.sub uri_path offset (String.length uri_path - offset)
      else
        ""
    else
      ""  (* If for some reason we're called incorrectly, fallback to empty string. *)
  in

  (* Build the full path inside "PUBLIC_DIR/assets". *)
  let file_path =
    Filename.concat public_dir (Filename.concat "assets" subpath)
  in

  (* Attempt to serve the file directly from disk. *)
  Lwt.catch
    (fun () ->
       Server.respond_file ~fname:file_path ()
    )
    (fun _ex ->
       (* If not found or any error, respond 404. *)
       Server.respond_string
         ~status:`Not_found
         ~body:"Not Found"
         ()
    )

