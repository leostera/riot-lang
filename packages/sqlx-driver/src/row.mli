open Std

(* Database row representation.

   A row is a collection of named fields with their corresponding values.
   This module provides convenient accessors for retrieving typed values
   from database rows.

   ## Example

   ```ocaml
   let row = [
     ("id", Value.int 1);
     ("name", Value.string "Alice");
     ("active", Value.bool true);
   ]

   match Row.int "id" row with
   | Some id -> Printf.printf "User ID: %d\n" id
   | None -> print_endline "No ID field found"
   ```
*)

(* The type of a database row, represented as a list of field name and value pairs *)
type t = (string * Value.t) list

(* ## Field Access *)

(* `get field row` returns the value associated with `field` in `row`, or `None` if the field doesn't exist *)
val get: string -> t -> Value.t option

(* `fields row` returns a list of all field names in the row *)
val fields: t -> string list

(* ## Typed Accessors

   These functions combine field lookup with type conversion, returning `None`
   if either the field doesn't exist or the value is not of the expected type.
*)

(* `int field row` returns the integer value of `field`, or `None` if the field doesn't exist or isn't an integer *)
val int: string -> t -> int option

(* `string field row` returns the string value of `field`, or `None` if the field doesn't exist or isn't a string *)
val string: string -> t -> string option

(* `bool field row` returns the boolean value of `field`, or `None` if the field doesn't exist or isn't a boolean *)
val bool: string -> t -> bool option

(* `float field row` returns the float value of `field`, or `None` if the field doesn't exist or isn't a float *)
val float: string -> t -> float option

(* `bytes field row` returns the bytes value of `field`, or `None` if the field doesn't exist or isn't bytes *)
val bytes: string -> t -> bytes option

(* `timestamp field row` returns the timestamp value of `field`, or `None` if the field doesn't exist or isn't a timestamp *)
val timestamp: string -> t -> DateTime.t option

(* ## Utility Functions *)

(* `to_string row` converts a row to its string representation *)
val to_string: t -> string

(* `equal a b` tests equality between two rows, comparing both field names and values *)
val equal: t -> t -> bool
