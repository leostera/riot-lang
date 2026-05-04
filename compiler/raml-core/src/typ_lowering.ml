open Std
open Std.Data
open Typ.Model
module Core = Core_ir

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

type 'value validation = ('value, error list) result

type lowered_binding = {
  export: Core.Export.t option;
  item: Core.Init_item.t;
}

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

let validation_map2 = fun left right f ->
  match (left, right) with
  | (Ok left, Ok right) -> Ok (f left right)
  | (Error left, Ok _) -> Error left
  | (Ok _, Error right) -> Error right
  | (Error left, Error right) -> Error (left @ right)

let validation_map3 = fun first second third f ->
  match (first, second, third) with
  | (Ok first, Ok second, Ok third) -> Ok (f first second third)
  | (Error first, Ok _, Ok _) -> Error first
  | (Ok _, Error second, Ok _) -> Error second
  | (Ok _, Ok _, Error third) -> Error third
  | (Error first, Error second, Ok _) -> Error (first @ second)
  | (Error first, Ok _, Error third) -> Error (first @ third)
  | (Ok _, Error second, Error third) -> Error (second @ third)
  | (Error first, Error second, Error third) -> Error (first @ second @ third)

let map_results = fun items f ->
  List.fold_right
    items
    (Ok [])
    (fun item acc -> validation_map2 (f item) acc (fun item acc -> item :: acc))

let source_kind_to_string = fun kind ->
  match kind with
  | Source_unit.Implementation -> "implementation"
  | Source_unit.Interface -> "interface"

let error_to_json = fun error ->
  match error with
  | UnsupportedSourceKind { kind } ->
      Json.obj
        [
          ("kind", Json.string "unsupported_source_kind");
          ("source_kind", Json.string (source_kind_to_string kind));
        ]
  | UnsupportedItem { item_id; kind; scope_path } ->
      let _ = item_id in
      Json.obj
        [
          ("kind", Json.string "unsupported_item");
          ("item_kind", Json.string kind);
          ("scope_path", Json.string (Surface_path.to_string scope_path));
        ]
  | MissingBinding { binding_id } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "missing_binding") ]
  | MissingExpr { expr_id } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "missing_expr") ]
  | MissingPattern { pattern_id } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "missing_pattern") ]
  | UnsupportedBinding { binding_id; reason } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "unsupported_binding"); ("reason", Json.string reason) ]
  | UnsupportedPattern { pattern_id; reason } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "unsupported_pattern"); ("reason", Json.string reason) ]
  | UnsupportedExpr { expr_id; reason } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason) ]
  | InvalidIntLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_int_literal"); ("literal", Json.string literal) ]
  | InvalidFloatLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_float_literal"); ("literal", Json.string literal) ]

let lower_binding = fun ~name ~scope_path ->
  match scope_path with
  | "" -> {
    export = Some Core.Export.{ name; symbol = Core.Entity_id.from_name name };
    item = Core.Init_item.Eval (Core.Expr.Constant Core.Constant.Unit)
  }
  | _ -> { export = None; item = Core.Init_item.Eval (Core.Expr.Constant Core.Constant.Unit) }

let lower_value_item = fun _value_item ->
  let binding = lower_binding ~name:"" ~scope_path:"" in
  Ok Core.Binding_group.{
    rec_flag = Core.Rec_flag.Nonrecursive;
    items = [ binding.item ];
    exports = Option.to_list binding.export
  }

let lower_item = fun semantic_tree item ->
  let _ = item in
  let _ = semantic_tree in
  Ok Core.Binding_group.{ rec_flag = Core.Rec_flag.Nonrecursive; items = []; exports = [] }

let unsupported_file = fun (source_unit: Source_unit.t) ->
  Error [ UnsupportedSourceKind { kind = source_unit.kind }; ]

let lower_file = fun ~(source_unit:Source_unit.t) (_semantic_tree: unit) ->
  match source_unit.kind with
  | Source_unit.Interface -> unsupported_file source_unit
  | Source_unit.Implementation -> Ok Core.Compilation_unit.{
    unit_id = Core.Unit_id.from_source_unit source_unit;
    exports = [];
    init = []
  }
