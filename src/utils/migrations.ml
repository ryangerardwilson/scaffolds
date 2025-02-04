open Lwt.Infix
open Sqlite3

let initiate_migrations () : bool Lwt.t =
  (* 1. Reads a file’s entire contents. *)
  let read_file_contents path =
    let ch = open_in path in
    Fun.protect
      (fun () -> really_input_string ch (in_channel_length ch))
      ~finally:(fun () -> close_in ch)
  in

  (* 2. Check if a CREATE TABLE statement corresponds to a table that exists in the DB. *)
  let table_is_compatible ~db ~table_sql =
    let statement = String.trim table_sql in
    let re =
      Str.regexp_case_fold
        "^[ \t]*CREATE[ \t]+TABLE[ \t]+\\(IF[ \t]+NOT[ \t]+EXISTS[ \t]+\\)?[\"'`]*\\([A-Za-z0-9_]+\\)[\"'`]*"
    in
    if Str.string_match re statement 0 then
      let table_name = Str.matched_group 2 statement in
      let lower_name = String.lowercase_ascii table_name in
      let sql =
        Printf.sprintf
          "SELECT name
           FROM sqlite_master
           WHERE type='table'
             AND name='%s';"
          lower_name
      in
      let found = ref false in
      let callback (row : row_not_null) (_hdrs : headers) =
        if Array.length row > 0 then found := true
      in
      ignore (exec_not_null db ~cb:callback sql);
      !found
    else
      false
  in

  (* 3. Retrieve a list of existing tables in the DB. *)
  let get_existing_tables db =
    let tbls = ref [] in
    let cb (row : row_not_null) (_headers : headers) =
      if Array.length row > 0 then
        tbls := row.(0) :: !tbls
    in
    ignore (exec_not_null db ~cb
      "SELECT name FROM sqlite_master WHERE type='table';");
    !tbls
  in

  (* 3b. Retrieve the CREATE TABLE statement for a given table. *)
  let get_table_schema db table_name =
    let schema = ref "" in
    let callback (row : row_not_null) (_headers : headers) =
      if Array.length row > 0 then
        schema := row.(0)
    in
    let query =
      Printf.sprintf
        "SELECT sql
         FROM sqlite_master
         WHERE type='table'
           AND name='%s';"
        table_name
    in
    ignore (Sqlite3.exec_not_null db ~cb:callback query);
    !schema
  in

  (* 4. Apply the entire schema from disk, printing info when new tables are created. *)
  let apply_schema db schema_sql =
    (* Split the schema into statements by semicolon. *)
    let statements = Str.split (Str.regexp ";") schema_sql in

    let total_statements = List.length statements - 1 in
    Printf.printf "[DEBUG] apply_schema: Found %d statements.\n%!" total_statements;

    (* A regex to detect CREATE TABLE statements and capture the table name. *)
    let re_create_table =
      Str.regexp_case_fold
        "^[ \t]*CREATE[ \t]+TABLE[ \t]+\\(IF[ \t]+NOT[ \t]+EXISTS[ \t]+\\)?[\"'`]*\\([A-Za-z0-9_]+\\)[\"'`]*"
    in

    (* Apply each non-blank statement individually, using iteri for the index. *)
    List.iteri (fun i stmt ->
      let trimmed = String.trim stmt in

      (* Skip empty statements (like trailing semicolons). *)
      if trimmed <> "" then begin
        Printf.printf
          "[DEBUG] Applying statement %d/%d:\n%s\n%!"
          (i + 1) total_statements
          trimmed;

        (* If the statement creates a table, print a message about it. *)
        if Str.string_match re_create_table trimmed 0 then begin
          let table_name = Str.matched_group 2 trimmed in
          Printf.printf "[INFO] Creating table: %s\n%!" table_name;
        end;

        (* Execute this single statement. *)
        match Sqlite3.exec db trimmed with
        | Sqlite3.Rc.OK ->
            Printf.printf
              "[DEBUG] Statement %d/%d executed successfully.\n%!"
              (i + 1) total_statements
        | err ->
            (* Print full debugging information. *)
            Printf.eprintf
              "[ERROR] Statement %d/%d failed.\n\
               [ERROR] While applying the following SQL:\n%s\n\
               SQLite returned error code: %s\n\
               Error message: %s\n%!"
              (i + 1) total_statements
              trimmed
              (Sqlite3.Rc.to_string err)
              (Sqlite3.errmsg db);

            (* Then raise an exception so the caller's catch block can handle it. *)
            failwith ""
      end
    ) statements
  in

  (* 5. Handle one folder => open/create DB => check/apply schema.sql. *)
  let handle_folder db_name =
    let db_dir = Filename.concat "dbs" db_name in
    let db_path = Filename.concat db_dir (db_name ^ ".db") in
    let schema_path = Filename.concat db_dir "schema.sql" in

    let db =
      let db = db_open db_path in
      match errcode db with
      | Rc.OK -> db
      | err ->
         failwith (Printf.sprintf "Failed to open DB '%s': %s"
                     db_path (Rc.to_string err))
    in

    let schema_exists = Sys.file_exists schema_path in
    if schema_exists then begin
      let existing_tables = get_existing_tables db in
      let schema_sql = read_file_contents schema_path in
      match existing_tables with
      | [] ->
         (* If no tables at all, apply schema. *)
         Printf.printf
           "[INFO] No tables found in '%s'; applying schema.\n%!"
           db_name;
         apply_schema db schema_sql
      | _::_ ->
         (* We have some tables => ensure the new schema is compatible. *)
         let statements = Str.split (Str.regexp ";") schema_sql in
         let incompatible =
           List.exists
             (fun stmt ->
                let trimmed = String.trim stmt in
                if trimmed = "" then
                  false
                else if String.lowercase_ascii trimmed
                        |> String.starts_with ~prefix:"create table"
                then
                  not (table_is_compatible ~db ~table_sql:trimmed)
                else
                  false)
             statements
         in
         if incompatible then begin
           (* Print all existing tables’ schemas. *)
           Printf.printf
             "\n[ERROR] Existing tables in '%s.db' are incompatible with schema.sql!\n\n"
             db_name;

           Printf.printf "Existing table definitions:\n";
           List.iter
             (fun tbl ->
                let tbl_schema = get_table_schema db tbl in
                Printf.printf "  -- Table: %s\n%s\n\n" tbl tbl_schema
             )
             existing_tables;

           Printf.printf "File schema (schema.sql):\n%s\n\n" schema_sql;

           failwith (Printf.sprintf
              "Schema incompatibility in '%s.db' with schema.sql!"
              db_name)
         end
    end;

    ignore (db_close db);
    Lwt.return_unit
  in

  (* 6. Discover subdirectories under "dbs". *)
  let read_db_names () =
    let dbs_dir = "dbs" in
    if not (Sys.file_exists dbs_dir) then
      failwith (Printf.sprintf "Directory '%s' does not exist!" dbs_dir);
    let is_subdir f =
      let full_path = Filename.concat dbs_dir f in
      Sys.is_directory full_path
    in
    Sys.readdir dbs_dir
    |> Array.to_list
    |> List.filter is_subdir
  in

  (* 7. For each DB folder, run handle_folder. *)
  Lwt.catch
    (fun () ->
      (* 7. For each DB folder, run handle_folder. *)
      let db_names = read_db_names () in
      Lwt_list.iter_s handle_folder db_names >|= fun () ->
      true
    )
    (fun ex ->
      (* Print the exception and return false on failure *)
      (* prerr_endline (Printexc.to_string ex); *)
      Lwt.return false
    )

