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

module Operand: sig
  type t =
    | Register of string
    | Global of string
    | Symbol_address of string
    | Literal of Literal.t
  val to_json: t -> Json.t
end

module Callee: sig
  type t =
    | Direct of string
    | Indirect of Operand.t
  val to_json: t -> Json.t
end

module Instruction: sig
  type t =
    | Label of string
    | Move of { dst: string; src: Operand.t }
    | Store_global of { symbol: string; src: Operand.t }
    | Call of { dst: string option; callee: Callee.t; arguments: Operand.t list }
    | Branch_if_zero of { operand: Operand.t; target: string }
    | Jump of string
    | Return of Operand.t option
    | Comment of string
  val to_json: t -> Json.t
end

module Procedure: sig
  type kind =
    | Function
    | Entry
  type t = {
    name: string;
    kind: kind;
    params: string list;
    body: Instruction.t list;
  }
  val kind_to_json: kind -> Json.t

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
    procedures: Procedure.t list;
    exports: Export.t list;
  }
  val empty: module_name:string -> t

  val to_json: t -> Json.t
end
