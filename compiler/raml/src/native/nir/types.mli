open Std
open Std.Data

module Literal: sig
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
  val to_json: t -> Json.t
end

module Expr: sig
  type callee =
    | Direct of string
    | Indirect of t

  and call = {
    callee: callee;
    arguments: t list;
  }

  and t =
    | Literal of Literal.t
    | Symbol of string
    | Call of call
  val callee_to_json: callee -> Json.t

  val call_to_json: call -> Json.t

  val to_json: t -> Json.t
end

module Function: sig
  type t = {
    name: string;
    params: string list;
    body: Expr.t;
  }
  val to_json: t -> Json.t
end

module Binding: sig
  type t = {
    name: string;
    expr: Expr.t;
  }
  val to_json: t -> Json.t
end

module Import_requirement: sig
  type linkage =
    | Runtime
    | External
  type t = {
    symbol: string;
    linkage: linkage;
  }
  val linkage_to_json: linkage -> Json.t

  val to_json: t -> Json.t
end

module Runtime_helper: sig
  type t = {
    name: string;
    symbol: string;
  }
  val to_json: t -> Json.t
end

module Entry_item: sig
  type t =
    | Binding of Binding.t
    | Eval of Expr.t
  val to_json: t -> Json.t
end

module Export: sig
  type t = {
    name: string;
    symbol: string;
  }
  val to_json: t -> Json.t
end

module Program: sig
  type t = {
    module_name: string;
    imports: Import_requirement.t list;
    runtime_helpers: Runtime_helper.t list;
    functions: Function.t list;
    entry: Entry_item.t list;
    exports: Export.t list;
  }
  val empty: module_name:string -> t

  val to_json: t -> Json.t
end
