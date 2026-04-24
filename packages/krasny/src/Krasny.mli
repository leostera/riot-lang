open Std

(** Formatter errors.

    `krasny` formats typed CSTs. If parsing required recovery or the current CST
    builder cannot lift the file, formatting fails instead of attempting to
    pretty-print a broken file. *)
type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_parse of Syn.Diagnostic.t Std.Collections.Vector.t
  | Cannot_lower of string
val format_error_to_string: format_error -> string

(** `format_error_to_string err` renders formatter failures into a concise
    human-readable string. *)

(** `format result` renders a parser2 result into formatted OCaml source.

    The current implementation lowers the supported Ast2 typed-view subset
    through an internal document tree before rendering to text. Files fail
    formatting when parser2 reports diagnostics or when the Ast2 surface does
    not yet expose enough structure for a purely structural lowering. Non-empty
    formatted output always ends with a final newline. *)
val format: Syn.Parser2.parse_result -> (string, format_error) result

(** `parse_source ~filename source` parses an OCaml source file with parser2,
    selecting implementation vs interface parsing from [filename]. *)
val parse_source: filename:Path.t -> string -> Syn.Parser2.parse_result

(** `format_source ~filename source` parses with parser2 and formats an OCaml
    source file, selecting implementation vs interface parsing from [filename]. *)
val format_source: filename:Path.t -> string -> (string, format_error) result

(** Deprecated alias for [format]. *)
val format2: Syn.Parser2.parse_result -> (string, format_error) result

(** `syntax_hash result` computes a normalized hash of the parsed concrete
    syntax tree, ignoring formatting-only punctuation and wrappers that
    `krasny` canonicalizes. *)
val syntax_hash: Syn.Parser2.parse_result -> string

(** `syntax_hash_source ~filename source` parses an OCaml source file and
    computes its normalized concrete syntax hash. *)
val syntax_hash_source: filename:Path.t -> string -> string

(** `write ~writer result` renders a parse result into the provided writer. *)
val write: writer:IO.Writer.t -> Syn.Parser2.parse_result -> (unit, [
    `Format of format_error
    | `Write of IO.error
  ]) result

module Doc: module type of Doc

module Solver: module type of Solver

module Printer: module type of Printer

module Lower: module type of Lower

module Lower2: module type of Lower2

module Runner: module type of Runner

module Report: module type of Report
