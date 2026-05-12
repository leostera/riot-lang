open Std
open Std.Data

type error =
  | UnsupportedModuleKind of { kind: Raml_core.Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }
val error_to_json: error -> Json.t

val lower_compilation_unit:
  context:Raml_core.Compilation_context.t ->
  Raml_core.Core_ir.Compilation_unit.t ->
  (Types.Program.t, error list) result
