open Std
open Std.Collections

type format_error = Format_core.format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_lower of string

module Doc = Doc
module Solver = Solver
module Printer = Printer
module Source = Source
module Lower = Lower
module Runner = Runner
module Report = Report

let format_error_to_string = Format_core.format_error_to_string
let format = Format_core.format

let syntax_hash = Runner.syntax_hash

let write ~writer result =
  match format result with
  | Error err -> Error (`Format err)
  | Ok formatted -> IO.write_all writer ~buf:formatted |> Result.map_error (fun err -> `Write err)
