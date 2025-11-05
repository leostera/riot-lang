(** ANSI Emitter - Convert matrix to terminal escape sequences *)

open Std

(** Convert a matrix to ANSI-formatted string for terminal output *)
val emit : Matrix.t -> string

(** Emit only the differences between two matrices (optimization) *)
val emit_diff : old:Matrix.t -> new_:Matrix.t -> string
