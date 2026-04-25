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
type write_error = Format_core.write_error =
  | Format_failed of format_error
  | Write_failed of IO.error

(** `format result` renders a parser1 result into formatted OCaml source.

    This preserves the historical public entrypoint for callers that still use
    [Syn.parse]. Non-empty formatted output always ends with a final newline. *)
val format: Syn.Parser.parse_result -> (string, format_error) result

(** `parse_source ~filename source` parses an OCaml source file with parser2,
    selecting implementation vs interface parsing from [filename]. *)
val parse_source: filename:Path.t -> string -> Syn.Parser2.parse_result

(** `format_source ~filename source` parses with parser2 and formats an OCaml
    source file, selecting implementation vs interface parsing from [filename]. *)
val format_source: filename:Path.t -> string -> (string, format_error) result

(** `format2 result` renders a parser2 result into formatted OCaml source. *)
val format2: Syn.Parser2.parse_result -> (string, format_error) result

(** `stream_format result ~writer ~width` renders a parser2 result directly into
    [writer] with the streaming formatter. *)
val stream_format:
  Syn.Parser2.parse_result -> writer:IO.Writer.t -> width:int -> (unit, write_error) result

(** `stream_format_to_string result ~width` renders a parser2 result with the
    streaming formatter and returns the formatted source. *)
val stream_format_to_string: Syn.Parser2.parse_result -> width:int -> (string, format_error) result

(** `syntax_hash result` computes a normalized hash of the parsed concrete
    syntax tree, ignoring formatting-only punctuation and wrappers that
    `krasny` canonicalizes. *)
val syntax_hash: Syn.Parser.parse_result -> string

(** `syntax_hash2 result` computes a normalized hash for a parser2 result. *)
val syntax_hash2: Syn.Parser2.parse_result -> string

(** `syntax_hash_source ~filename source` parses an OCaml source file and
    computes its normalized concrete syntax hash. *)
val syntax_hash_source: filename:Path.t -> string -> string

(** `write ~writer result` renders a parse result into the provided writer. *)
val write: writer:IO.Writer.t -> Syn.Parser2.parse_result -> (unit, write_error) result

module Doc: module type of Doc

module Stream_doc: module type of Stream_doc

module Streaming_lower: module type of Streaming_lower

module Solver: module type of Solver

module Printer: module type of Printer

module Lower: module type of Lower

module Lower2: module type of Lower2

module Runner: module type of Runner

module Report: module type of Report
