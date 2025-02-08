(* database.ml *)

open Sqlite3
open Printf
open Owl

(* -------------------------------
   Custom “basic” dataframe representation.
--------------------------------- *)
type cell =
  | Int of int64
  | Float of float
  | Text of string
  | Null

type dataframe = {
  columns : string list;
  rows    : cell list list;
}

(* -------------------------------
   The unified result type.
   When calling query_to_df, if df_type = "owl", we wrap the Owl DataFrame;
   otherwise we return our basic dataframe.
--------------------------------- *)
type query_df_result =
  | Basic_df of dataframe
  | Owl_df of Dataframe.t

(* -------------------------------
   Helper: Convert a SQLite cell into a string.
   (Used for building the Owl DataFrame.)
--------------------------------- *)
let cell_to_string cell =
  match cell with
  | Int i   -> Int64.to_string i
  | Float f -> string_of_float f
  | Text s  -> s
  | Null    -> "NULL"

(* -------------------------------
   Helper: Transpose a list of lists.
   Given a list of rows (each a list of values), return a list of columns.
--------------------------------- *)
let rec transpose (matrix : 'a list list) : 'a list list =
  match matrix with
  | [] -> []
  | [] :: _ -> []
  | _ -> (List.map List.hd matrix) :: transpose (List.map List.tl matrix)

(* -------------------------------
   query_to_df:
     - database_name: Assumes the DB file is at "dbs/<database_name>/<database_name>.db"
     - table_name: Reserved for future use (validation/dynamic construction)
     - query_str: The SQL query to run.
     - df_type: If string "owl" then return an Owl DataFrame; otherwise a basic DF.
--------------------------------- *)
let query_to_df (database_name : string)
                (table_name : string)
                (query_str : string)
                (df_type : string)
                : query_df_result =
  let db_path = sprintf "dbs/%s/%s.db" database_name database_name in
  let db = db_open db_path in
  if errcode db <> Rc.OK then
    failwith (sprintf "Could not open DB: %s" db_path)
  else
    let stmt = prepare db query_str in
    if errcode db <> Rc.OK then
      failwith "Error preparing statement"
    else
      let col_count = column_count stmt in
      let column_names =
        List.init col_count (fun i -> column_name stmt i)
      in
      (* Fetch rows converting each field to our cell type.
         We add a catch-all branch to handle any unexpected data tag. *)
      let rec fetch_rows acc =
        match step stmt with
        | Rc.ROW ->
            let row =
              List.init col_count (fun i ->
                match column stmt i with
                | Data.NULL         -> Null
                | Data.INT i        -> Int i
                | Data.FLOAT f      -> Float f
                | Data.TEXT s       -> Text s
                | Data.BLOB _       -> Text "<blob>"
                | _                 -> Text "<unknown>"
              )
            in
            fetch_rows (acc @ [row])
        | Rc.DONE -> acc
        | rc -> failwith (sprintf "Error stepping statement: %s" (Rc.to_string rc))
      in
      let rows = fetch_rows [] in
      ignore (finalize stmt);
      ignore (db_close db);
      
      if df_type = "owl" then
        begin
          (* Convert each cell to string *)
          let rows_as_strings = List.map (List.map cell_to_string) rows in
          let columns_data =
            if rows_as_strings = [] then
              (* No data: create an empty array for each column *)
              List.map (fun _ -> [||]) column_names
            else
              let list_of_columns = transpose rows_as_strings in
              List.map Array.of_list list_of_columns
          in
          (* Convert the list of columns into an array *)
          let columns_array = Array.of_list columns_data in
          (* For each column array, pack it as a string series. *)
          let series_array =
            Array.map Dataframe.pack_string_series columns_array
          in
          (* Create the Owl DataFrame using Dataframe.make.
             It expects an array of column names and a ~data array of series. *)
          let owl_df =
            Dataframe.make (Array.of_list column_names) ~data:series_array
          in
          Owl_df owl_df
        end
      else
        Basic_df { columns = column_names; rows = rows }

(* -------------------------------
   Insert method:
     - database_name: Assumes DB file at "dbs/<database_name>/<database_name>.db"
     - table_name: Table for the new record.
     - details: A list of (column, value) pairs for the new row.
--------------------------------- *)
let insert (database_name : string)
           (table_name : string)
           (details : (string * string) list)
           : unit =
  let db_path = sprintf "dbs/%s/%s.db" database_name database_name in
  let db = db_open db_path in
  if errcode db <> Rc.OK then
    failwith (sprintf "Could not open DB: %s" db_path)
  else
    let columns = List.map fst details in
    let placeholders = String.concat ", " (List.map (fun _ -> "?") details) in
    let query_str = sprintf "INSERT INTO %s (%s) VALUES (%s)"
                      table_name
                      (String.concat ", " columns)
                      placeholders
    in
    let stmt = prepare db query_str in
    List.iteri (fun i (_, value) ->
      ignore (bind_text stmt (i + 1) value)
    ) details;
    (match step stmt with
     | Rc.DONE -> ()
     | rc -> failwith (sprintf "Error inserting record into %s:%s: %s"
                           database_name table_name (Rc.to_string rc)));
    ignore (finalize stmt);
    ignore (db_close db)

(* End of database.ml *)

