open Lwt
open Yojson.Basic

(* Exception for short-circuiting in main.ml *)
exception DumpAndDie of string

(*******************************************************************************
  USAGE NOTE:
    1) Use Debugger.any for each value you want to dump, then pass them
       as a list to dump_and_die.
    2) main.ml should wrap route handling in Lwt.catch to catch DumpAndDie.
*******************************************************************************)

(* Helper to wrap any OCaml value into Obj.t so we can unify different types. *)
let any (x : 'a) : Obj.t =
  Obj.magic x

(*
  dump_and_die : Obj.t list -> 'b Lwt.t

  - Accepts a list of universal Obj.t, one for each value we want to dump.
  - Converts each to JSON using a rudimentary reflection approach.
  - Produces a JSON array of all dumped values under "dump".
  - Raises DumpAndDie so that main.ml can catch and return the debug info.
*)
let dump_and_die (values : Obj.t list) =
  (* Tag constants often used in OCaml for blocks. Actual values may vary. *)
  let double_tag = 253 in
  let string_tag = 252 in

  (* Local helpers for reflection via Obj. *)
  let is_int_obj x = Obj.is_int x in

  let is_empty_list x =
    (Obj.tag x = 0) && (Obj.magic x == [])
  in

  let is_nonempty_list x =
    (Obj.tag x = 0) && (Obj.magic x != [])
  in

  let unroll_list (x : Obj.t) : Obj.t list =
    let rec loop acc v =
      if is_empty_list v then List.rev acc
      else if is_nonempty_list v then
        let hd = Obj.field v 0 in
        let tl = Obj.field v 1 in
        loop (hd :: acc) tl
      else
        List.rev acc
    in
    loop [] x
  in

  let is_pair x =
    Obj.tag x = 0 && Obj.size x = 2
  in

  let extract_pair x =
    let a = Obj.field x 0 in
    let b = Obj.field x 1 in
    (a, b)
  in

  let is_string_obj x =
    Obj.tag x = string_tag
  in

  (* True if something is specifically (string * string). *)
  let is_pair_of_strings x =
    if not (is_pair x) then false
    else
      let (l, r) = extract_pair x in
      is_string_obj l && is_string_obj r
  in

  (* Recursively convert one Obj.t item to Yojson.Basic.t. *)
  let rec item_to_yojson (x : Obj.t) : Yojson.Basic.t =
    if is_int_obj x then
      `Int (Obj.magic x)
    else
      let t = Obj.tag x in
      (* Is it a float block? *)
      if t = double_tag then
        `Float (Obj.magic x)
      (* Is it a string block? *)
      else if t = string_tag then
        `String (Obj.magic x)
      else if is_empty_list x then
        `List []
      else if is_nonempty_list x then
        let items = unroll_list x in
        (* If every item is (string*string), make an array of {key: value} objects. *)
        if List.for_all is_pair_of_strings items then
          let mapped =
            List.map (fun pair_obj ->
              let (lk, rv) = extract_pair pair_obj in
              (`Assoc [ (Obj.magic lk, `String (Obj.magic rv)) ])
            ) items
          in
          `List mapped
        else
          `List (List.map item_to_yojson items)
      else if is_pair x then
        let (left, right) = extract_pair x in
        let left_json  = item_to_yojson left in
        let right_json = item_to_yojson right in
        (* If both sides are strings, produce { left: right }. Else [left, right]. *)
        match left_json, right_json with
        | `String s1, `String s2 ->
            `Assoc [ (s1, `String s2) ]
        | _ ->
            `List [left_json; right_json]
      else
        `String "Unhandled/complex structure (using Obj)."
  in

  (* 
  (* Convert each Obj.t to JSON, collect in a list. *)
  let json_items = List.map item_to_yojson values in
  (* Wrap them in {"dump": [...]} *)
  let top_json = `Assoc [ "dump", `List json_items ] in
  let body_str = Yojson.Basic.pretty_to_string top_json in

  (* Raise exception so main.ml can short-circuit this request. *)
  Lwt.fail (DumpAndDie body_str)
  *)
  (* Convert each item to JSON *)
  let json_items = List.map item_to_yojson values in
  (* Convert the list of JSON objects to a Yojson array *)
  let json_list = `List json_items in
  (* Generate a JSON string from the list *)
  let json_string = Yojson.Basic.to_string json_list in


  Renderer.server_side_render
    "debugger.html"  (* the file you placed in PUBLIC_DIR *)
    [ ("{{DATA_TO_DEBUG}}", json_string) ]
  >>= fun (_response, body) ->
    (* Convert Cohttp body to string *)
    Cohttp_lwt.Body.to_string body >>= fun final_html ->
    Lwt.fail (DumpAndDie final_html)


