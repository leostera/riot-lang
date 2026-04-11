open Std
open Std.Data

module Unit_id: sig
  type t = {
    relpath: Path.t;
    unit_name: string;
    kind: Source_unit.kind;
  }
  val of_source_unit: Source_unit.t -> t

  val to_json: t -> Json.t
end

module Rec_flag: sig
  type t =
    | Nonrecursive
    | Recursive
  val to_json: t -> Json.t
end

module Constant: sig
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Char of string
    | String of string
  val to_json: t -> Json.t
end

module Expr: sig
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
  val apply_callee_to_json: apply_callee -> Json.t

  val apply_to_json: apply -> Json.t

  val lambda_to_json: lambda -> Json.t

  val binding_to_json: binding -> Json.t

  val let_to_json: let_ -> Json.t

  val sequence_to_json: sequence -> Json.t

  val tuple_to_json: tuple -> Json.t

  val tuple_get_to_json: tuple_get -> Json.t

  val if_then_else_to_json: if_then_else -> Json.t

  val primitive_to_json: primitive -> Json.t

  val to_json: t -> Json.t
end

module Binding: sig
  type t = {
    name: string;
    expr: Expr.t;
  }
  val to_json: t -> Json.t
end

module Export: sig
  type t = {
    name: string;
    symbol: string;
  }
  val to_json: t -> Json.t
end

module Init_item: sig
  type t =
    | Binding of Binding.t
    | Eval of Expr.t
  val to_json: t -> Json.t
end

module Binding_group: sig
  type t = {
    rec_flag: Rec_flag.t;
    items: Init_item.t list;
    exports: Export.t list;
  }
  val to_json: t -> Json.t
end

module Compilation_unit: sig
  type t = {
    unit_id: Unit_id.t;
    exports: Export.t list;
    init: Binding_group.t list;
  }
  val empty: Unit_id.t -> t

  val to_json: t -> Json.t
end
