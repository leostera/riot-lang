open Std

(**
   Database row representation.

   A row is a collection of named fields with their corresponding values.

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
type t = (string * Value.t) list

(** Return the value associated with `field`, or `None` when the field is absent. *)
val get: string -> t -> Value.t option

(** Return all field names in the row. *)
val fields: t -> string list

(** Return the integer value of `field`, or `None` when absent or not an integer. *)
val int: string -> t -> int option

(** Return the string value of `field`, or `None` when absent or not a string. *)
val string: string -> t -> string option

(** Return the boolean value of `field`, or `None` when absent or not a boolean. *)
val bool: string -> t -> bool option

(** Return the float value of `field`, or `None` when absent or not a float. *)
val float: string -> t -> float option

(** Return the bytes value of `field`, or `None` when absent or not bytes. *)
val bytes: string -> t -> bytes option

(** Return the timestamp value of `field`, or `None` when absent or not a timestamp. *)
val timestamp: string -> t -> DateTime.t option

(** Render a row as a string. *)
val to_string: t -> string

(** Compare two rows for equality, including field names and values. *)
val equal: t -> t -> bool
