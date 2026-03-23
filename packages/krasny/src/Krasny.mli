open Std

(** Formatter errors.

    `krasny` formats typed CSTs. If parsing required recovery or the current CST
    builder cannot lift the file, formatting fails instead of attempting to
    pretty-print a broken file. *)
type format_error =
  | Cannot_build_cst of Syn.build_cst_error

(** `format result` renders a parse result into formatted OCaml source.

    The current implementation handles a small CST-driven subset and returns
    the original source for valid-but-unsupported files. Broken files fail
    because formatting requires a successful CST lift. *)
val format : Syn.Parser.parse_result -> (string, format_error) result

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
  (unit, [ `Format of format_error | `Write of 'err ]) result
