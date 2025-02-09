open Cohttp_lwt_unix
open Lwt.Infix

(* Now update your server_side_render function so that after reading the file,
   we process both wrappers and embeds and then perform the substitutions. *)
let server_side_render (filename : string)
                       (substitutions : (string * string) list)
                       : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =

  (* Helper: safely get PUBLIC_DIR (defaults to "dist") *)
  let public_dir () =
    Sys.getenv_opt "PUBLIC_DIR" |> Option.value ~default:"dist"
  in

  (* Helper: read a file asynchronously *)
  let file_read path =
    Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read
  in


  (* Process wrapper tags.
     This regex expects an opening tag like:
       <wrapper-base-template>
     and a closing tag that either exactly matches the opening name or adds an extra “s”
       </wrapper-base-template>  or  </wrapper-base-templates>
  *)
  let process_wrappers (content : string) : string Lwt.t =
    let wrapper_regexp =
      Str.regexp "<wrapper-\\([a-zA-Z0-9_-]+\\)>\\(\\(.\\|\n\\)*\\)</wrapper-\\1s?>" in
    if Str.string_match wrapper_regexp content 0 then
      let wrapper_name = Str.matched_group 1 content in
      let inner_content = Str.matched_group 2 content in
      (* Build path e.g. "dist/components/wrappers/base-template.html" *)
      let wrapper_path =
        Filename.concat (public_dir ())
          (Filename.concat "components/wrappers" (wrapper_name ^ ".html")) in
      Lwt.catch
        (fun () ->
           file_read wrapper_path >>= fun wrapper_html ->
           (* In the wrapper HTML, look for the literal "<embed>" placeholder.
              Replace it with the inner content from the wrapper tag. *)
           let wrapped =
             Str.global_replace (Str.regexp_string "<embed />") inner_content wrapper_html in
           Lwt.return wrapped)
        (fun _exn ->
           (* On error, simply return the inner content *)
           Lwt.return inner_content)
    else
      Lwt.return content
  in

  (* Process embed tags.
     This function looks for embed tags of the form:
       <embed-main-content />  or  <embed-main-content>
     It accepts optional whitespace before the optional self-closing slash.
  *)
  let process_embeds (content : string) : string Lwt.t =
    let embed_regexp = Str.regexp "<embed-\\([a-zA-Z0-9_-]+\\)\\s*/? />" in
    let parts = Str.full_split embed_regexp content in
    let process_part part =
      match part with
      | Str.Text s -> Lwt.return s
      | Str.Delim s ->
        if Str.string_match embed_regexp s 0 then (
          let embed_name = Str.matched_group 1 s in
          let embed_path =
            Filename.concat (public_dir ())
              (Filename.concat "components/embeds" (embed_name ^ ".html")) in
          Lwt.catch
            (fun () -> file_read embed_path)
            (fun _exn ->
               Lwt.return ("<!-- failed to load embed " ^ embed_name ^ " -->"))
        ) else
          Lwt.return s
    in
    Lwt_list.map_p process_part parts >|= String.concat ""
  in

  let file_path = Filename.concat (public_dir ()) filename in
  if Sys.file_exists file_path then
    file_read file_path >>= fun initial_content ->
    (* First, see if the content uses a wrapper mechanism: *)
    process_wrappers initial_content >>= fun wrapped_content ->
    (* Then process any embed tags that can exist in the resulting content: *)
    process_embeds wrapped_content >>= fun content_with_embeds ->
    (* Finally, do your plain-old string substitutions: *)
    let final_content =
      List.fold_left
        (fun acc (needle, replacement) ->
           Str.global_replace (Str.regexp_string needle) replacement acc)
        content_with_embeds
        substitutions
    in
    Server.respond_string ~status:`OK ~body:final_content ()
  else
    Server.respond_string
      ~status:`Not_found
      ~body:"File not found"
      ()

