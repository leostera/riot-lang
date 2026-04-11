module Core = Raml_core.Core_ir

module Import: sig
  type kind =
    | Runtime
    | Host
  type t = {
    module_name: string;
    name: string;
    kind: kind;
  }
  val key: t -> string

  val to_json: t -> Std.Data.Json.t
end

module Runtime_plan: sig
  type t = {
    function_table_elements: Core.Entity_id.t list;
    has_indirect_calls: bool;
    needs_closure_runtime: bool;
  }
  val empty: t

  val to_json: t -> Std.Data.Json.t
end

module Primitive_kind: sig
  type t =
    | Pure
    | Runtime
    | Host_import
  val to_json: t -> Std.Data.Json.t
end

module Expr: sig
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
  val to_json: t -> Std.Data.Json.t
end

module Function: sig
  type t = {
    entity_id: Core.Entity_id.t;
    name: string;
    params: Expr.param list;
    body: Expr.t;
  }
  val to_json: t -> Std.Data.Json.t
end

module Global: sig
  type t = {
    entity_id: Core.Entity_id.t;
    name: string;
    expr: Expr.t;
  }
  val to_json: t -> Std.Data.Json.t
end

module Init_item: sig
  type t =
    | Global of Global.t
    | Eval of Expr.t
  val to_json: t -> Std.Data.Json.t
end

module Compilation_unit: sig
  type t = {
    unit_id: Core.Unit_id.t;
    imports: Import.t list;
    runtime_plan: Runtime_plan.t;
    globals: Global.t list;
    functions: Function.t list;
    init: Init_item.t list;
    exports: Core.Export.t list;
  }
  val to_json: t -> Std.Data.Json.t
end
