open Std

(** Formatter errors.

    `krasny` formats typed CSTs. If parsing required recovery or the current CST
    builder cannot lift the file, formatting fails instead of attempting to
    pretty-print a broken file. *)
type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_lower of string
val format_error_to_string: format_error -> string

(** `format_error_to_string err` renders formatter failures into a concise
    human-readable string. *)

(** `format result` renders a parse result into formatted OCaml source.

    The current implementation lowers the supported CST subset through an
    internal document tree before rendering to text. Files fail formatting
    when the current CST surface does not yet expose enough structure for a
    purely structural lowering. Broken files also fail because formatting
    requires a successful CST lift. Non-empty formatted output always ends
    with a final newline. *)
val format: Syn.Parser.parse_result -> (string, format_error) result

(** `format2 result` renders a parser2 result through the experimental Ast2
    typed-view lowering path. It is intentionally side-by-side with [format]
    while parser2 and the view layer are validated against formatter tests and
    benchmarks. *)
val format2: Syn.Parser2.parse_result -> (string, format_error) result

(** `syntax_hash result` computes a normalized hash of the parsed concrete
    syntax tree, ignoring formatting-only punctuation and wrappers that
    `krasny` canonicalizes. *)
val syntax_hash: Syn.Parser.parse_result -> string

(** `write ~writer result` renders a parse result into the provided writer. *)
val write: writer:IO.Writer.t -> Syn.Parser.parse_result -> (unit, [
    `Format of format_error
    | `Write of IO.error
  ]) result

module Runner: module type of Runner

module Report: module type of Report
