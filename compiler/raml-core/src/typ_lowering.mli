open Std
open Std.Data
open Typ.Model

type error =
  | UnsupportedSourceKind of { kind: Source_unit.kind }
  | UnsupportedItem of { item_id: int; kind: string; scope_path: Surface_path.t }
  | MissingBinding of { binding_id: Binding_id.t }
  | MissingExpr of { expr_id: int }
  | MissingPattern of { pattern_id: int }
  | UnsupportedBinding of { binding_id: Binding_id.t; reason: string }
  | UnsupportedPattern of { pattern_id: int; reason: string }
  | UnsupportedExpr of { expr_id: int; reason: string }
  | InvalidIntLiteral of { expr_id: int; literal: string }
  | InvalidFloatLiteral of { expr_id: int; literal: string }
val error_to_json: error -> Json.t

val lower_file: source_unit:Source_unit.t -> unit -> (Core_ir.Compilation_unit.t, error list) result
