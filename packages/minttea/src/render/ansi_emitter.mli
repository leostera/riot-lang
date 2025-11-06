(** ANSI Emitter - Convert matrix to terminal escape sequences *)

open Std

(** Render mode controls how many rows are emitted *)
type render_mode = 
  | Fullscreen  (** Emit all rows in the matrix *)
  | ContentFit  (** Only emit rows with actual content *)

(** Convert a matrix to ANSI-formatted string for terminal output *)
val emit : Matrix.t -> mode:render_mode -> string

(** Emit only the differences between two matrices (optimization) *)
val emit_diff : old:Matrix.t -> new_:Matrix.t -> mode:render_mode -> string
