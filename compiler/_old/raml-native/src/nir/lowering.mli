open Std
open Std.Data

module Compiler_source_unit = Raml_core.Source_unit

module Core = Raml_core.Core_ir

type error =
  | UnsupportedModuleKind of { kind: Compiler_source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }
type pass_snapshot = {
  name: string;
  program: Types.Program.t;
}
type trace = {
  initial: Types.Program.t;
  passes: pass_snapshot list;
  final: Types.Program.t;
}
val error_to_json: error -> Json.t

val trace_to_json: trace -> Json.t

val lower_compilation_unit_with_trace: Core.Compilation_unit.t -> (trace, error list) result

val lower_compilation_unit: Core.Compilation_unit.t -> (Types.Program.t, error list) result
