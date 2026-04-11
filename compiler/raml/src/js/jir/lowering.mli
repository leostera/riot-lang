open Std
open Std.Data

module Source_unit = RamlCore.Source_unit
module Core = RamlCore.CoreIR

type error =
  | UnsupportedModuleKind of { kind: Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }
val error_to_json: error -> Json.t

val lower_compilation_unit: Core.Compilation_unit.t -> (Types.Program.t, error list) result
