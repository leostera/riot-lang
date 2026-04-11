open Std
open Std.Data

let source_kind_to_json = fun kind ->
  match kind with
  | Source_unit.Implementation -> Json.string "implementation"
  | Source_unit.Interface -> Json.string "interface"

module Unit_id = struct
  type t = {
    relpath: Path.t;
    unit_name: string;
    kind: Source_unit.kind;
  }

  let of_source_unit = fun (source_unit: Source_unit.t) ->
    { relpath = source_unit.relpath; unit_name = source_unit.unit_name; kind = source_unit.kind }

  let to_json = fun unit_id ->
    Json.obj
      [
        ("relpath", Json.string (Path.to_string unit_id.relpath));
        ("unit_name", Json.string unit_id.unit_name);
        ("kind", source_kind_to_json unit_id.kind);
      ]
end

module Rec_flag = struct
  type t =
    | Nonrecursive
    | Recursive

  let to_json = fun rec_flag ->
    match rec_flag with
    | Nonrecursive -> Json.string "nonrecursive"
    | Recursive -> Json.string "recursive"
end

module Constant = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Char of string
    | String of string

  let to_json = fun constant ->
    match constant with
    | Unit -> Json.obj [ ("kind", Json.string "unit") ]
    | Bool value -> Json.obj [ ("kind", Json.string "bool"); ("value", Json.bool value); ]
    | Int value -> Json.obj [ ("kind", Json.string "int"); ("value", Json.int value); ]
    | Float value -> Json.obj [ ("kind", Json.string "float"); ("value", Json.float value); ]
    | Char value -> Json.obj [ ("kind", Json.string "char"); ("value", Json.string value); ]
    | String value -> Json.obj [ ("kind", Json.string "string"); ("value", Json.string value); ]
end

module Expr = struct
  type apply_callee =
    | Direct of string
    | Indirect of t

  and apply = {
    callee: apply_callee;
    arguments: t list;
  }

  and lambda = {
    params: string list;
    body: t;
  }

  and binding = {
    name: string;
    expr: t;
  }

  and let_ = {
    rec_flag: Rec_flag.t;
    bindings: binding list;
    body: t;
  }

  and sequence = {
    first: t;
    second: t;
  }

  and tuple = t list

  and tuple_get = {
    tuple: t;
    index: int;
  }

  and if_then_else = {
    condition: t;
    then_: t;
    else_: t;
  }

  and primitive = {
    name: string;
    arguments: t list;
  }

  and t =
    | Constant of Constant.t
    | Var of string
    | Apply of apply
    | Lambda of lambda
    | Let of let_
    | Sequence of sequence
    | Tuple of tuple
    | Tuple_get of tuple_get
    | If_then_else of if_then_else
    | Primitive of primitive

  let rec apply_callee_to_json = fun callee ->
    match callee with
    | Direct function_name -> Json.obj
      [ ("kind", Json.string "direct"); ("function", Json.string function_name); ]
    | Indirect expr -> Json.obj [ ("kind", Json.string "indirect"); ("expr", to_json expr); ]

  and apply_to_json = fun (apply: apply) ->
    Json.obj
      [
        ("callee", apply_callee_to_json apply.callee);
        ("arguments", Json.array (List.map to_json apply.arguments));
      ]

  and lambda_to_json = fun (lambda: lambda) ->
    Json.obj
      [ ("params", Json.array (List.map Json.string lambda.params)); ("body", to_json lambda.body); ]

  and binding_to_json = fun (binding: binding) ->
    Json.obj [ ("name", Json.string binding.name); ("expr", to_json binding.expr); ]

  and let_to_json = fun (let_: let_) ->
    Json.obj
      [
        ("rec_flag", Rec_flag.to_json let_.rec_flag);
        ("bindings", Json.array (List.map binding_to_json let_.bindings));
        ("body", to_json let_.body);
      ]

  and sequence_to_json = fun (sequence: sequence) ->
    Json.obj [ ("first", to_json sequence.first); ("second", to_json sequence.second); ]

  and tuple_to_json = fun (tuple: tuple) ->
    Json.obj [ ("elements", Json.array (List.map to_json tuple)); ]

  and tuple_get_to_json = fun (tuple_get: tuple_get) ->
    Json.obj [ ("tuple", to_json tuple_get.tuple); ("index", Json.int tuple_get.index); ]

  and if_then_else_to_json = fun (if_then_else: if_then_else) ->
    Json.obj
      [
        ("condition", to_json if_then_else.condition);
        ("then", to_json if_then_else.then_);
        ("else", to_json if_then_else.else_);
      ]

  and primitive_to_json = fun (primitive: primitive) ->
    Json.obj
      [
        ("name", Json.string primitive.name);
        ("arguments", Json.array (List.map to_json primitive.arguments));
      ]

  and to_json = fun expr ->
    match expr with
    | Constant constant -> Json.obj
      [ ("kind", Json.string "constant"); ("constant", Constant.to_json constant); ]
    | Var name -> Json.obj [ ("kind", Json.string "var"); ("name", Json.string name); ]
    | Apply apply -> Json.obj [ ("kind", Json.string "apply"); ("apply", apply_to_json apply); ]
    | Lambda lambda -> Json.obj
      [ ("kind", Json.string "lambda"); ("lambda", lambda_to_json lambda); ]
    | Let let_ -> Json.obj [ ("kind", Json.string "let"); ("let", let_to_json let_); ]
    | Sequence sequence -> Json.obj
      [ ("kind", Json.string "sequence"); ("sequence", sequence_to_json sequence); ]
    | Tuple tuple -> Json.obj [ ("kind", Json.string "tuple"); ("tuple", tuple_to_json tuple); ]
    | Tuple_get tuple_get -> Json.obj
      [ ("kind", Json.string "tuple_get"); ("tuple_get", tuple_get_to_json tuple_get); ]
    | If_then_else if_then_else -> Json.obj
      [ ("kind", Json.string "if_then_else"); ("if_then_else", if_then_else_to_json if_then_else); ]
    | Primitive primitive -> Json.obj
      [ ("kind", Json.string "primitive"); ("primitive", primitive_to_json primitive); ]
end

module Binding = struct
  type t = {
    name: string;
    expr: Expr.t;
  }

  let to_json = fun binding ->
    Json.obj [ ("name", Json.string binding.name); ("expr", Expr.to_json binding.expr); ]
end

module Export = struct
  type t = {
    name: string;
    symbol: string;
  }

  let to_json = fun export ->
    Json.obj [ ("name", Json.string export.name); ("symbol", Json.string export.symbol); ]
end

module Init_item = struct
  type t =
    | Binding of Binding.t
    | Eval of Expr.t

  let to_json = fun item ->
    match item with
    | Binding binding -> Json.obj
      [ ("kind", Json.string "binding"); ("binding", Binding.to_json binding); ]
    | Eval expr -> Json.obj [ ("kind", Json.string "eval"); ("expr", Expr.to_json expr); ]
end

module Binding_group = struct
  type t = {
    rec_flag: Rec_flag.t;
    items: Init_item.t list;
    exports: Export.t list;
  }

  let to_json = fun group ->
    Json.obj
      [
        ("rec_flag", Rec_flag.to_json group.rec_flag);
        ("items", Json.array (List.map Init_item.to_json group.items));
        ("exports", Json.array (List.map Export.to_json group.exports));
      ]
end

module Compilation_unit = struct
  type t = {
    unit_id: Unit_id.t;
    exports: Export.t list;
    init: Binding_group.t list;
  }

  let empty = fun unit_id -> { unit_id; exports = []; init = [] }

  let to_json = fun compilation_unit ->
    Json.obj
      [
        ("unit_id", Unit_id.to_json compilation_unit.unit_id);
        ("exports", Json.array (List.map Export.to_json compilation_unit.exports));
        ("init", Json.array (List.map Binding_group.to_json compilation_unit.init));
      ]
end
