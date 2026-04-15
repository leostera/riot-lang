open Std
open Std.Data
module Core = Raml_core.Core_ir

module Import = struct
  type kind =
    | Runtime
    | Host

  type t = {
    module_name: string;
    name: string;
    kind: kind;
  }

  let key = fun import -> import.module_name ^ ":" ^ import.name

  let kind_to_json = fun kind ->
    match kind with
    | Runtime -> Json.string "runtime"
    | Host -> Json.string "host"

  let to_json = fun import ->
    Json.obj
      [
        ("module_name", Json.string import.module_name);
        ("name", Json.string import.name);
        ("kind", kind_to_json import.kind);
      ]
end

module Runtime_plan = struct
  type t = {
    function_table_elements: Core.Entity_id.t list;
    has_indirect_calls: bool;
    needs_closure_runtime: bool;
  }

  let empty = {
    function_table_elements = [];
    has_indirect_calls = false;
    needs_closure_runtime = false
  }

  let to_json = fun plan ->
    Json.obj
      [
        (
          "function_table_elements",
          Json.array (List.map ~fn:Core.Entity_id.to_json plan.function_table_elements)
        );
        ("has_indirect_calls", Json.bool plan.has_indirect_calls);
        ("needs_closure_runtime", Json.bool plan.needs_closure_runtime);
      ]
end

module Primitive_kind = struct
  type t =
    | Pure
    | Runtime
    | Host_import

  let to_json = fun kind ->
    match kind with
    | Pure -> Json.string "pure"
    | Runtime -> Json.string "runtime"
    | Host_import -> Json.string "host_import"
end

module Expr = struct
  type direct_call = {
    callee: Core.Entity_id.t;
    arguments: t list;
  }

  and indirect_call = {
    callee: t;
    arguments: t list;
  }

  and param = {
    entity_id: Core.Entity_id.t;
    name: string;
  }

  and lambda = {
    params: param list;
    body: t;
  }

  and binding = {
    entity_id: Core.Entity_id.t;
    name: string;
    expr: t;
  }

  and let_ = {
    rec_flag: Core.Rec_flag.t;
    bindings: binding list;
    body: t;
  }

  and sequence = {
    first: t;
    second: t;
  }

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
    primitive: Core.Primitive.t;
    kind: Primitive_kind.t;
    arguments: t list;
  }

  and t =
    | Constant of Core.Constant.t
    | Var of Core.Entity_id.t
    | Direct_call of direct_call
    | Indirect_call of indirect_call
    | Lambda of lambda
    | Let of let_
    | Sequence of sequence
    | Tuple of t list
    | Tuple_get of tuple_get
    | If_then_else of if_then_else
    | Primitive of primitive

  let param_to_json = fun param ->
    Json.obj
      [ ("entity_id", Core.Entity_id.to_json param.entity_id); ("name", Json.string param.name); ]

  let rec to_json = fun expr ->
    match expr with
    | Constant constant -> Json.obj
      [ ("kind", Json.string "constant"); ("constant", Core.Constant.to_json constant); ]
    | Var entity_id -> Json.obj
      [ ("kind", Json.string "var"); ("entity_id", Core.Entity_id.to_json entity_id); ]
    | Direct_call call -> Json.obj
      [
        ("kind", Json.string "direct_call");
        ("callee", Core.Entity_id.to_json call.callee);
        ("arguments", Json.array (List.map ~fn:to_json call.arguments));
      ]
    | Indirect_call call -> Json.obj
      [
        ("kind", Json.string "indirect_call");
        ("callee", to_json call.callee);
        ("arguments", Json.array (List.map ~fn:to_json call.arguments));
      ]
    | Lambda lambda -> Json.obj
      [
        ("kind", Json.string "lambda");
        ("params", Json.array (List.map ~fn:param_to_json lambda.params));
        ("body", to_json lambda.body);
      ]
    | Let let_ -> Json.obj
      [
        ("kind", Json.string "let");
        ("rec_flag", Core.Rec_flag.to_json let_.rec_flag);
        ("bindings", Json.array (List.map ~fn:binding_to_json let_.bindings));
        ("body", to_json let_.body);
      ]
    | Sequence sequence -> Json.obj
      [
        ("kind", Json.string "sequence");
        ("first", to_json sequence.first);
        ("second", to_json sequence.second);
      ]
    | Tuple elements -> Json.obj
      [ ("kind", Json.string "tuple"); ("elements", Json.array (List.map ~fn:to_json elements)); ]
    | Tuple_get tuple_get -> Json.obj
      [
        ("kind", Json.string "tuple_get");
        ("tuple", to_json tuple_get.tuple);
        ("index", Json.int tuple_get.index);
      ]
    | If_then_else if_then_else -> Json.obj
      [
        ("kind", Json.string "if_then_else");
        ("condition", to_json if_then_else.condition);
        ("then", to_json if_then_else.then_);
        ("else", to_json if_then_else.else_);
      ]
    | Primitive primitive -> Json.obj
      [
        ("kind", Json.string "primitive");
        ("name", Core.Primitive.to_json primitive.primitive);
        ("primitive_kind", Primitive_kind.to_json primitive.kind);
        ("arguments", Json.array (List.map ~fn:to_json primitive.arguments));
      ]

  and binding_to_json = fun binding ->
    Json.obj
      [
        ("entity_id", Core.Entity_id.to_json binding.entity_id);
        ("name", Json.string binding.name);
        ("expr", to_json binding.expr);
      ]
end

module Function = struct
  type t = {
    entity_id: Core.Entity_id.t;
    name: string;
    params: Expr.param list;
    body: Expr.t;
  }

  let to_json = fun function_ ->
    Json.obj
      [
        ("entity_id", Core.Entity_id.to_json function_.entity_id);
        ("name", Json.string function_.name);
        ("params", Json.array (List.map ~fn:Expr.param_to_json function_.params));
        ("body", Expr.to_json function_.body);
      ]
end

module Global = struct
  type t = {
    entity_id: Core.Entity_id.t;
    name: string;
    expr: Expr.t;
  }

  let to_json = fun global ->
    Json.obj
      [
        ("entity_id", Core.Entity_id.to_json global.entity_id);
        ("name", Json.string global.name);
        ("expr", Expr.to_json global.expr);
      ]
end

module Init_item = struct
  type t =
    | Global of Global.t
    | Eval of Expr.t

  let to_json = fun item ->
    match item with
    | Global global -> Json.obj
      [ ("kind", Json.string "global"); ("global", Global.to_json global); ]
    | Eval expr -> Json.obj [ ("kind", Json.string "eval"); ("expr", Expr.to_json expr); ]
end

module Compilation_unit = struct
  type t = {
    unit_id: Core.Unit_id.t;
    imports: Import.t list;
    runtime_plan: Runtime_plan.t;
    globals: Global.t list;
    functions: Function.t list;
    init: Init_item.t list;
    exports: Core.Export.t list;
  }

  let to_json = fun compilation_unit ->
    Json.obj
      [
        ("unit_id", Core.Unit_id.to_json compilation_unit.unit_id);
        ("imports", Json.array (List.map ~fn:Import.to_json compilation_unit.imports));
        ("runtime_plan", Runtime_plan.to_json compilation_unit.runtime_plan);
        ("globals", Json.array (List.map ~fn:Global.to_json compilation_unit.globals));
        ("functions", Json.array (List.map ~fn:Function.to_json compilation_unit.functions));
        ("init", Json.array (List.map ~fn:Init_item.to_json compilation_unit.init));
        ("exports", Json.array (List.map ~fn:Core.Export.to_json compilation_unit.exports));
      ]
end
