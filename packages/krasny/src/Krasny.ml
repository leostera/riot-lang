open Std
open Std.Collections

type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_lower of string

module Doc = Doc
module Solver = Solver
module Printer = Printer
module Lower = Lower
module Lower2 = Lower2
module Runner = Runner
module Report = Report

let format_error_to_string = Format_core.format_error_to_string

let format = Format_core.format

let format2 = Format_core.format2

let syntax_hash = Runner.syntax_hash

let write = fun ~writer result ->
  match format result with
  | Error err -> Error (`Format err)
  | Ok formatted ->
      let buffer = IO.Buffer.from_string formatted in
      IO.write_all writer ~from:buffer |> Result.map_err ~fn:(fun err -> `Write err)
