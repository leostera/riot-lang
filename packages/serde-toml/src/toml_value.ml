open Std

type t =
  | String of string
  | Int of int64
  | Float of float
  | Bool of bool
  | Array of t list
  | Table of (string * t) list

let is_table = fun __tmp1 ->
  match __tmp1 with
  | Table _ -> true
  | _ -> false

let is_array_of_tables = fun __tmp1 ->
  match __tmp1 with
  | Array values -> List.for_all is_table values
  | _ -> false

let is_empty_table = fun __tmp1 ->
  match __tmp1 with
  | Table [] -> true
  | _ -> false
