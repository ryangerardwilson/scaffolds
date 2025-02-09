(* lib/Landing.ml *)

(* Handle landing page requests *)
let handle_landing _conn req _body =
  let username = Authentication.get_username_if_user_is_logged_in req in

  let logged_in_as_html =
    match username with
    | Some username_string -> Printf.sprintf "Logged in as %s" username_string
    | None -> ""
  in

  let link_block_html =
    match username with
    | Some _ ->
      {|<nav class="flex items-center space-x-2">
<a href="/dashboard" class="text-blue-500 hover:text-blue-700 font-semibold">Dashboard</a>
<span class="text-gray-500">|</span>
<a href="/about" class="text-blue-500 hover:text-blue-700 font-semibold">About</a>
<span class="text-gray-500">|</span>
<a href="/logout" class="text-blue-500 hover:text-blue-700 font-semibold">Logout</a>
</nav>|}
    | None ->
      {|<nav class="flex items-center space-x-2">
<a href="/signup" class="text-green-500 hover:text-green-700 font-semibold">Sign Up</a>
<span class="text-gray-500">|</span>
<a href="/login" class="text-green-500 hover:text-green-700 font-semibold">Login</a>
<span class="text-gray-500">|</span>
<a href="/about" class="text-blue-500 hover:text-blue-700 font-semibold">About</a>
</nav>|}
  in


  let filename = "landing.html" in
  let app_name = Sys.getenv "APP_NAME" in

  let substitutions = [
    ("{{APP_NAME}}", app_name);
    ("{{LOGGED_IN_AS}}", logged_in_as_html);
    ("{{LINK_BLOCK}}", link_block_html);
  ] in

  let input_list = [Debugger.any req] in
  let output_list = [Debugger.any filename; Debugger.any substitutions] in
  Lwt.async (fun () -> Debugger.log_event "/ => handle_landing" input_list output_list);

  Renderer.server_side_render filename substitutions

