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

module Slot: sig
  type t = {
    index: int;
    offset: int;
  }
  val to_json: t -> Json.t
end

module Home: sig
  type t =
    | Register of string
    | Stack_slot of Slot.t
  val to_json: t -> Json.t
end

module Destination: sig
  type t =
    | Register of string
    | Home of Home.t
  val to_json: t -> Json.t
end

module Operand: sig
  type t =
    | Register of string
    | Home of Home.t
    | Global of string
    | Symbol_address of string
    | Literal of Literal.t
  val to_json: t -> Json.t
end

module Home_binding: sig
  type t = {
    name: string;
    home: Home.t;
  }
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
    | Move of { dst: Destination.t; src: Operand.t }
    | Store_global of { symbol: string; src: Operand.t }
    | Call of { dst: Destination.t option; callee: Callee.t; arguments: Operand.t list }
    | Branch_if_zero of { operand: Operand.t; target: string }
    | Jump of string
    | Return of Operand.t option
    | Comment of string
  val to_json: t -> Json.t
end

module Frame: sig
  type t = {
    contains_calls: bool;
    frame_required: bool;
    slots: Slot.t list;
    homes: Home_binding.t list;
    saved_registers: string list;
    frame_size: int;
  }
  val empty: t

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
    frame: Frame.t;
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
