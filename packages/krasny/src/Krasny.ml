open Std
open Std.Collections

type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_parse of Syn.Diagnostic.t Vector.t
  | Cannot_lower of string

type write_error = Format_core.write_error =
  | Format_failed of format_error
  | Write_failed of IO.error

module Doc = Doc
module Stream_doc = Stream_doc
module Streaming_lower = Streaming_lower
module Solver = Solver
module Printer = Printer
module Lower = Lower
module Lower2 = Lower2
module Runner = Runner
module Report = Report

let format_error_to_string = Format_core.format_error_to_string

let format = Format_core.format

let parse_source = Format_core.parse_source

let format_source = Format_core.format_source

let format2 = Format_core.format2

let stream_format = Format_core.stream_format

let stream_format_to_string = Format_core.stream_format_to_string

let syntax_hash = Runner.syntax_hash

let syntax_hash2 = Runner.syntax_hash2

let syntax_hash_source = fun ~filename source -> parse_source ~filename source |> syntax_hash2

let write = Format_core.write
