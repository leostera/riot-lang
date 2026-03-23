open Std

(** `format result` renders a parse result into formatted OCaml source.

    The initial implementation is intentionally conservative and lossless: it
    renders the exact token stream from the parsed tree. Later versions will
    lower through a proper formatting document IR, but this API shape already
    gives callers a single owned entrypoint. *)
val format : Syn.Parser.parse_result -> string

(** `syntax_hash result` computes a whitespace-insensitive hash of the concrete
    syntax tree.

    This hash ignores source positions entirely and skips whitespace trivia, but
    it still includes comments, token text, and syntax-node structure. It is
    useful for round-trip formatter invariants like parse -> format -> parse,
    where layout may change but the concrete syntax should not. *)
val syntax_hash : Syn.Parser.parse_result -> string

(** `write ~writer result` renders a parse result into the provided writer. *)
val write :
  writer:('dst, 'err) IO.Writer.t ->
  Syn.Parser.parse_result ->
  (unit, 'err) result
