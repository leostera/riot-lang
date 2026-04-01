open Std
open Std.Iter

type t =
  | Cursor: {
      id: string;
      driver: (module Sqlx_driver.Driver.Intf with type result_set = 'rs);
      mutable result_set: 'rs;
      mutable exhausted: bool;
      mutable row_count: int;
    } -> t

let make = fun (type rs) id (result_set: rs) (
  driver: (module Sqlx_driver.Driver.Intf with type result_set = rs)
) ->
  Cursor {
    id;
    driver;
    result_set;
    exhausted = false;
    row_count = 0;
  }

module RowIterator = struct
  type state = {
    cursor: t;
    mutable exhausted: bool;
  }

  type item = Sqlx_driver.Row.t

  let next = fun state ->
    if state.exhausted then
      None
    else
      let (Cursor cursor) = state.cursor in
      if cursor.exhausted then
        (
          state.exhausted <- true;
          None
        )
      else
        let module D = (val cursor.driver) in
        match D.fetch_row cursor.result_set with
        | Some row ->
            cursor.row_count <- cursor.row_count + 1;
            Some row
        | None ->
            cursor.exhausted <- true;
            state.exhausted <- true;
            None

  let size = fun state ->
    if state.exhausted then
      0
    else
      let (Cursor cursor) = state.cursor in
      let module D = (val cursor.driver) in
      D.rows_affected cursor.result_set

  let clone = fun state -> {cursor = state.cursor;exhausted = state.exhausted;}
end

let to_mut_iter = fun cursor ->
  MutIterator.make (module RowIterator) {RowIterator.cursor;exhausted = false;}

let fetch_one = fun cursor ->
  let iter = to_mut_iter cursor in
  MutIterator.next iter

let fetch_many = fun cursor n ->
  let iter = to_mut_iter cursor in
  let rec take acc remaining =
    if remaining <= 0 then
      List.rev acc
    else
      match MutIterator.next iter with
      | None -> List.rev acc
      | Some row -> take (row :: acc) (remaining - 1)
  in
  take [] n

let fetch_all = fun cursor -> cursor |> to_mut_iter |> MutIterator.to_list

let id = fun (Cursor cursor) -> cursor.id

let row_count = fun (Cursor cursor) -> cursor.row_count

let is_exhausted = fun (Cursor cursor) -> cursor.exhausted
