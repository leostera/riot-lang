open Std

(** `format result` renders a parse result into formatted OCaml source.

    The initial implementation is intentionally conservative and lossless: it
    renders the exact token stream from the parsed tree. Later versions will
    lower through a proper formatting document IR, but this API shape already
    gives callers a single owned entrypoint. *)
val format : Syn.Parser.parse_result -> string

(** `write ~writer result` renders a parse result into the provided writer. *)
val write :
  writer:('dst, 'err) IO.Writer.t ->
  Syn.Parser.parse_result ->
  (unit, 'err) result
