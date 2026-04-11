open Std
open Std.Data

module Literal = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | String of string

  let to_json = fun literal ->
    match literal with
    | Unit -> Json.obj [ ("kind", Json.string "unit") ]
    | Bool value -> Json.obj [ ("kind", Json.string "bool"); ("value", Json.bool value); ]
    | Int value -> Json.obj [ ("kind", Json.string "int"); ("value", Json.int value); ]
    | Float value -> Json.obj [ ("kind", Json.string "float"); ("value", Json.float value); ]
    | String value -> Json.obj [ ("kind", Json.string "string"); ("value", Json.string value); ]
end

module Operand = struct
  type t =
    | Register of string
    | Global of string
    | Symbol_address of string
    | Literal of Literal.t

  let to_json = fun operand ->
    match operand with
    | Register name -> Json.obj [ ("kind", Json.string "register"); ("name", Json.string name); ]
    | Global name -> Json.obj [ ("kind", Json.string "global"); ("name", Json.string name); ]
    | Symbol_address name -> Json.obj
      [ ("kind", Json.string "symbol_address"); ("name", Json.string name); ]
    | Literal literal -> Json.obj
      [ ("kind", Json.string "literal"); ("literal", Literal.to_json literal); ]
end

module Callee = struct
  type t =
    | Direct of string
    | Indirect of Operand.t

  let to_json = fun callee ->
    match callee with
    | Direct name -> Json.obj [ ("kind", Json.string "direct"); ("name", Json.string name); ]
    | Indirect operand -> Json.obj
      [ ("kind", Json.string "indirect"); ("operand", Operand.to_json operand); ]
end

module Instruction = struct
  type t =
    | Label of string
    | Move of { dst: string; src: Operand.t }
    | Store_global of { symbol: string; src: Operand.t }
    | Call of { dst: string option; callee: Callee.t; arguments: Operand.t list }
    | Branch_if_zero of { operand: Operand.t; target: string }
    | Jump of string
    | Return of Operand.t option
    | Comment of string

  let to_json = fun instruction ->
    match instruction with
    | Label name -> Json.obj [ ("kind", Json.string "label"); ("name", Json.string name); ]
    | Move { dst; src } -> Json.obj
      [ ("kind", Json.string "move"); ("dst", Json.string dst); ("src", Operand.to_json src); ]
    | Store_global { symbol; src } -> Json.obj
      [
        ("kind", Json.string "store_global");
        ("symbol", Json.string symbol);
        ("src", Operand.to_json src);
      ]
    | Call { dst; callee; arguments } -> Json.obj
      [
        ("kind", Json.string "call");
        ("dst", Option.map Json.string dst |> Option.unwrap_or ~default:Json.null);
        ("callee", Callee.to_json callee);
        ("arguments", Json.array (List.map Operand.to_json arguments));
      ]
    | Branch_if_zero { operand; target } -> Json.obj
      [
        ("kind", Json.string "branch_if_zero");
        ("operand", Operand.to_json operand);
        ("target", Json.string target);
      ]
    | Jump target -> Json.obj [ ("kind", Json.string "jump"); ("target", Json.string target); ]
    | Return operand -> Json.obj
      [
        ("kind", Json.string "return");
        ("operand", Option.map Operand.to_json operand |> Option.unwrap_or ~default:Json.null);
      ]
    | Comment text -> Json.obj [ ("kind", Json.string "comment"); ("text", Json.string text); ]
end

module Slot = struct
  type t = {
    name: string;
    offset: int;
  }

  let to_json = fun slot ->
    Json.obj [ ("name", Json.string slot.name); ("offset", Json.int slot.offset); ]
end

module Frame = struct
  type t = {
    contains_calls: bool;
    frame_required: bool;
    slots: Slot.t list;
    frame_size: int;
  }

  let empty = { contains_calls = false; frame_required = false; slots = []; frame_size = 0 }

  let to_json = fun frame ->
    Json.obj
      [
        ("contains_calls", Json.bool frame.contains_calls);
        ("frame_required", Json.bool frame.frame_required);
        ("slots", Json.array (List.map Slot.to_json frame.slots));
        ("frame_size", Json.int frame.frame_size);
      ]
end

module Procedure = struct
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

  let kind_to_json = fun kind ->
    match kind with
    | Function -> Json.string "function"
    | Entry -> Json.string "entry"

  let to_json = fun procedure ->
    Json.obj
      [
        ("name", Json.string procedure.name);
        ("kind", kind_to_json procedure.kind);
        ("params", Json.array (List.map Json.string procedure.params));
        ("frame", Frame.to_json procedure.frame);
        ("body", Json.array (List.map Instruction.to_json procedure.body));
      ]
end

module Export = struct
  type t = {
    name: string;
    symbol: string;
  }

  let to_json = fun export ->
    Json.obj [ ("name", Json.string export.name); ("symbol", Json.string export.symbol); ]
end

module Program = struct
  type t = {
    module_name: string;
    procedures: Procedure.t list;
    exports: Export.t list;
  }

  let empty = fun ~module_name -> { module_name; procedures = []; exports = [] }

  let to_json = fun program ->
    Json.obj
      [
        ("module_name", Json.string program.module_name);
        ("procedures", Json.array (List.map Procedure.to_json program.procedures));
        ("exports", Json.array (List.map Export.to_json program.exports));
      ]
end
