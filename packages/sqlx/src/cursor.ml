open Std

type t =
  | Cursor : {
      id : string;
      driver : (module Sqlx_driver.Driver.Intf with type result_set = 'rs);
      mutable result_set : 'rs;
      mutable exhausted : bool;
      mutable row_count : int;
    }
      -> t

let make (type rs) id (result_set : rs)
    (driver : (module Sqlx_driver.Driver.Intf with type result_set = rs)) =
  Cursor { id; driver; result_set; exhausted = false; row_count = 0 }

let fetch_one (Cursor cursor) =
  if cursor.exhausted then None
  else
    let module D = (val cursor.driver) in
    match D.fetch_row cursor.result_set with
    | Some row ->
        cursor.row_count <- cursor.row_count + 1;
        Some row
    | None ->
        cursor.exhausted <- true;
        None

let fetch_many cursor count =
  let rec collect acc n =
    if n <= 0 then List.rev acc
    else
      match fetch_one cursor with
      | None -> List.rev acc
      | Some row -> collect (row :: acc) (n - 1)
  in
  collect [] count

let fetch_all cursor =
  let rec collect acc =
    match fetch_one cursor with
    | None -> List.rev acc
    | Some row -> collect (row :: acc)
  in
  collect []

let id (Cursor cursor) = cursor.id
let row_count (Cursor cursor) = cursor.row_count
let is_exhausted (Cursor cursor) = cursor.exhausted
