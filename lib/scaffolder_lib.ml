(* AUTO-GENERATED by compile.py Step 0 *)

let ensure_dir path =
  if Sys.file_exists path then
    Printf.printf "[INFO] scaffolds - Directory '%s' already exists, skipping creation.\n" path
  else begin
    Unix.mkdir path 0o755;
    Printf.printf "[INFO] scaffolds - Created directory: %s\n" path
  end

let rec ensure_full_path path =
  if not (Sys.file_exists path) then begin
    ensure_full_path (Filename.dirname path);
    Unix.mkdir path 0o755;
    Printf.printf "[INFO] scaffolds - Created directory: %s\n" path
  end

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc;
  Printf.printf "[INFO] scaffolds - Created or updated file: %s\n" filename

let scaffold target_dir =
  ensure_full_path target_dir;

  let full_path sub_path = Filename.concat target_dir sub_path in

  write_file (full_path "main.ml") Templates_lib.file_main_ext_ml;

  write_file (full_path ".env") Templates_lib.ext_env;

  write_file (full_path "compile.sh") Templates_lib.file_compile_ext_sh;

  ensure_full_path (full_path "utils");
  write_file (full_path "utils/renderer.ml") Templates_lib.dir_utils_file_renderer_ext_ml;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/about.html") Templates_lib.dir_resources_file_about_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/home.html") Templates_lib.dir_resources_file_home_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/login.html") Templates_lib.dir_resources_file_login_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/landing.html") Templates_lib.dir_resources_file_landing_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/dashboard.html") Templates_lib.dir_resources_file_dashboard_ext_html;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/home.ml") Templates_lib.dir_lib_file_home_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/about.ml") Templates_lib.dir_lib_file_about_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/auth.ml") Templates_lib.dir_lib_file_auth_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/session.ml") Templates_lib.dir_lib_file_session_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/dashboard.ml") Templates_lib.dir_lib_file_dashboard_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/landing.ml") Templates_lib.dir_lib_file_landing_ext_ml;

  (* Make compile.sh executable *)
  let compile_sh_path = full_path "compile.sh" in
  if Sys.file_exists compile_sh_path then (
    Unix.chmod compile_sh_path 0o755;
    Printf.printf "[INFO] scaffolds - Set +x on compile.sh\n"
  );

  print_endline "[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile.";
