open Std

(**
   Database value types.

   These values are the shared representation that database drivers convert to
   and from their native wire values.

   ```ocaml
   let id_val = Value.int 42
   let name_val = Value.string "Alice"
   let active_val = Value.bool true

   match Value.to_int id_val with
   | Some n -> Printf.printf "ID: %d\n" n
   | None -> print_endline "Not an integer"
   ```
*)
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

(** Construct a SQL `NULL` value. *)
val null: t

(** Construct an integer value. *)
val int: int -> t

(** Construct a 64-bit integer value. *)
val int64: int64 -> t

(** Construct a 16-bit integer value. *)
val int16: int -> t

(** Construct a string value. *)
val string: string -> t

(** Construct a boolean value. *)
val bool: bool -> t

(** Construct a floating-point value. *)
val float: float -> t

(** Construct a byte-string value. *)
val bytes: bytes -> t

(** Construct a timestamp value. *)
val timestamp: DateTime.t -> t

(** Construct a timestamp-with-time-zone value. *)
val timestamp_with_timezone: DateTime.t -> t

(** Construct a date value as `year`, `month`, and `day`. *)
val date: int -> int -> int -> t

(** Construct a time value as `hour`, `minute`, `second`, and `microsecond`. *)
val time: int -> int -> int -> int -> t

(** Construct a UUID value. *)
val uuid: string -> t

(** Construct a JSON value. *)
val json: string -> t

(** Construct a numeric value. *)
val numeric: string -> t

(** Return an integer value, or `None` when the value has another type. *)
val to_int: t -> int option

(** Return a 64-bit integer value, or `None` when the value has another type. *)
val to_int64: t -> int64 option

(** Return a 16-bit integer value, or `None` when the value has another type. *)
val to_int16: t -> int option

(** Return a string value, or `None` when the value has another type. *)
val to_string_value: t -> string option

(** Return a boolean value, or `None` when the value has another type. *)
val to_bool: t -> bool option

(** Return a floating-point value, or `None` when the value has another type. *)
val to_float: t -> float option

(** Return a byte-string value, or `None` when the value has another type. *)
val to_bytes: t -> bytes option

(** Return a timestamp value, or `None` when the value has another type. *)
val to_timestamp: t -> DateTime.t option

(** Return a timestamp-with-time-zone value, or `None` when the value has another type. *)
val to_timestamp_with_timezone: t -> DateTime.t option

(** Return a date value, or `None` when the value has another type. *)
val to_date: t -> (int * int * int) option

(** Return a time value, or `None` when the value has another type. *)
val to_time: t -> (int * int * int * int) option

(** Return a UUID value, or `None` when the value has another type. *)
val to_uuid: t -> string option

(** Return a JSON value, or `None` when the value has another type. *)
val to_json: t -> string option

(** Return a numeric value, or `None` when the value has another type. *)
val to_numeric: t -> string option

(** Return `true` when the value is `NULL`. *)
val is_null: t -> bool

(** Render a value as a string. *)
val to_string: t -> string

(** Compare two values for equality. *)
val equal: t -> t -> bool

(** Compare two values for ordering. *)
val compare: t -> t -> Order.t
