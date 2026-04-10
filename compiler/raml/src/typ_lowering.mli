open Std
open Std.Data

type error =
  | UnsupportedSourceKind of { kind: Source_unit.kind }
  | UnsupportedItem of {
      item_id: Typ.Model.ItemArenaId.t;
      kind: string;
      scope_path: Typ.Model.SurfacePath.t
    }
  | MissingBinding of { binding_id: Typ.Model.BindingArenaId.t }
  | MissingExpr of { expr_id: Typ.Model.ExprArenaId.t }
  | MissingPattern of { pattern_id: Typ.Model.PatternArenaId.t }
  | UnsupportedBinding of { binding_id: Typ.Model.BindingArenaId.t; reason: string }
  | UnsupportedPattern of { pattern_id: Typ.Model.PatternArenaId.t; reason: string }
  | UnsupportedExpr of { expr_id: Typ.Model.ExprArenaId.t; reason: string }
  | InvalidIntLiteral of { expr_id: Typ.Model.ExprArenaId.t; literal: string }
  | InvalidFloatLiteral of { expr_id: Typ.Model.ExprArenaId.t; literal: string }
val error_to_json: error -> Json.t

val lower_file:
  source_unit:Source_unit.t ->
  Typ.Model.SemanticTree.file ->
  (Core_ir.Compilation_unit.t, error list) result
