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

  write_file (full_path ".gitignore") Templates_lib.ext_gitignore;

  write_file (full_path ".tailwind_build_input") Templates_lib.ext_tailwind_build_input;

  write_file (full_path "compile.sh") Templates_lib.file_compile_ext_sh;

  ensure_full_path (full_path "dbs/auth");
  write_file (full_path "dbs/auth/schema.sql") Templates_lib.dir_dbs_dir_auth_file_schema_ext_sql;

  ensure_full_path (full_path "utils");
  write_file (full_path "utils/migrations.ml") Templates_lib.dir_utils_file_migrations_ext_ml;

  ensure_full_path (full_path "utils");
  write_file (full_path "utils/renderer.ml") Templates_lib.dir_utils_file_renderer_ext_ml;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/about.html") Templates_lib.dir_resources_file_about_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/login.html") Templates_lib.dir_resources_file_login_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/landing.html") Templates_lib.dir_resources_file_landing_ext_html;

  ensure_full_path (full_path "resources");
  write_file (full_path "resources/dashboard.html") Templates_lib.dir_resources_file_dashboard_ext_html;

  ensure_full_path (full_path "resources/assets");
  write_file (full_path "resources/assets/styles.css") Templates_lib.dir_resources_dir_assets_file_styles_ext_css;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/login.ml") Templates_lib.dir_lib_file_login_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/logout.ml") Templates_lib.dir_lib_file_logout_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/about.ml") Templates_lib.dir_lib_file_about_ext_ml;

  ensure_full_path (full_path "lib");
  write_file (full_path "lib/assets.ml") Templates_lib.dir_lib_file_assets_ext_ml;

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
