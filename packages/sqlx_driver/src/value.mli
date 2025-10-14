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
  | Null (* SQL NULL value *)
  | Int of int (* Integer value *)
  | Float of float (* Floating point value *)
  | String of string (* Text/VARCHAR value *)
  | Bool of bool (* Boolean value *)
  | Bytes of bytes (* Binary data/BLOB value *)
  | Timestamp of Time.Instant.t (* Timestamp/DateTime value *)

(* ## Constructors *)

(* `null` creates a NULL value *)
val null : t

(* `int n` creates an integer value *)
val int : int -> t

(* `string s` creates a string value *)
val string : string -> t

(* `bool b` creates a boolean value *)
val bool : bool -> t

(* `float f` creates a floating-point value *)
val float : float -> t

(* `bytes b` creates a binary data value *)
val bytes : bytes -> t

(* `timestamp t` creates a timestamp value *)
val timestamp : Time.Instant.t -> t

(* ## Conversions *)

(* `to_int v` extracts an integer from a value, returning `None` if the value is not an integer *)
val to_int : t -> int option

(* `to_string_value v` extracts a string from a value, returning `None` if the value is not a string *)
val to_string_value : t -> string option

(* `to_bool v` extracts a boolean from a value, returning `None` if the value is not a boolean *)
val to_bool : t -> bool option

(* `to_float v` extracts a float from a value, returning `None` if the value is not a float *)
val to_float : t -> float option

(* `to_bytes v` extracts bytes from a value, returning `None` if the value is not bytes *)
val to_bytes : t -> bytes option

(* `to_timestamp v` extracts a timestamp from a value, returning `None` if the value is not a timestamp *)
val to_timestamp : t -> Time.Instant.t option

(* `is_null v` returns `true` if the value is NULL *)
val is_null : t -> bool

(* ## Utility Functions *)

(* `to_string v` converts a value to its string representation *)
val to_string : t -> string

(* `equal a b` tests equality between two values *)
val equal : t -> t -> bool

(* `compare a b` compares two values for ordering *)
val compare : t -> t -> int
