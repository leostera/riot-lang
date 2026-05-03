open Std

(* Database value types.

   This module defines the core value types that can be stored in and retrieved
   from SQL databases. All database drivers must convert their native types
   to and from these universal value types.

   ## Example

   ```ocaml
   let id_val = Value.int 42
   let name_val = Value.string "Alice"
   let active_val = Value.bool true

   match Value.to_int id_val with
   | Some n -> Printf.printf "ID: %d\n" n
   | None -> print_endline "Not an integer"
   ```
*)

(* The type of database values *)
type t =
  | Null
  | Int of int
  | Int64 of int64
  | Int16 of int
  | Float of float
  | String of string
  | Bool of bool
  | Bytes of bytes
  | Timestamp of DateTime.t
  | TimestampWithTimezone of DateTime.t
  | Date of int * int * int
  | Time of int * int * int * int
  | Uuid of string
  | Json of string
  | Numeric of string

(* ## Constructors *)
val null: t

val int: int -> t

val int64: int64 -> t

val int16: int -> t

val string: string -> t

val bool: bool -> t

val float: float -> t

val bytes: bytes -> t

val timestamp: DateTime.t -> t

val timestamp_with_timezone: DateTime.t -> t

val date: int -> int -> int -> t

val time: int -> int -> int -> int -> t

val uuid: string -> t

val json: string -> t

val numeric: string -> t

(* ## Conversions *)
val to_int: t -> int option

val to_int64: t -> int64 option

val to_int16: t -> int option

val to_string_value: t -> string option

val to_bool: t -> bool option

val to_float: t -> float option

val to_bytes: t -> bytes option

val to_timestamp: t -> DateTime.t option

val to_timestamp_with_timezone: t -> DateTime.t option

val to_date: t -> (int * int * int) option

val to_time: t -> (int * int * int * int) option

val to_uuid: t -> string option

val to_json: t -> string option

val to_numeric: t -> string option

val is_null: t -> bool

(* ## Utility Functions *)

(* `to_string v` converts a value to its string representation *)
val to_string: t -> string

(* `equal a b` tests equality between two values *)
val equal: t -> t -> bool

(* `compare a b` compares two values for ordering *)
val compare: t -> t -> Order.t
