open Std
open Std.Data

type error =
  | UnsupportedModuleKind of { kind: Raml.Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }
val error_to_json: error -> Json.t

val lower_compilation_unit: Raml.CoreIR.Compilation_unit.t -> (Types.Program.t, error list) result
