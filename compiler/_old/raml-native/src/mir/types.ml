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
    | Move of { dst: string; src: Operand.t }
    | Store_global of { symbol: string; src: Operand.t }
    | Call of { dst: string option; callee: Callee.t; arguments: Operand.t list }
    | If_then_else of if_then_else
    | Return of Operand.t option
    | Comment of string

  and if_then_else = {
    condition: Operand.t;
    then_: t list;
    else_: t list;
  }

  let rec if_then_else_to_json = fun if_then_else ->
    Json.obj
      [
        ("condition", Operand.to_json if_then_else.condition);
        ("then", Json.array (List.map ~fn:to_json if_then_else.then_));
        ("else", Json.array (List.map ~fn:to_json if_then_else.else_));
      ]

  and to_json = fun instruction ->
    match instruction with
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
        ("dst", Option.map dst ~fn:Json.string |> Option.unwrap_or ~default:Json.null);
        ("callee", Callee.to_json callee);
        ("arguments", Json.array (List.map arguments ~fn:Operand.to_json));
      ]
    | If_then_else if_then_else -> Json.obj
      [ ("kind", Json.string "if_then_else"); ("if_then_else", if_then_else_to_json if_then_else); ]
    | Return operand -> Json.obj
      [
        ("kind", Json.string "return");
        ("operand", Option.map operand ~fn:Operand.to_json |> Option.unwrap_or ~default:Json.null);
      ]
    | Comment text -> Json.obj [ ("kind", Json.string "comment"); ("text", Json.string text); ]
end

module Procedure = struct
  type kind =
    | Function
    | Entry

  type t = {
    name: string;
    kind: kind;
    params: string list;
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
        ("params", Json.array (List.map ~fn:Json.string procedure.params));
        ("body", Json.array (List.map ~fn:Instruction.to_json procedure.body));
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
        ("procedures", Json.array (List.map program.procedures ~fn:Procedure.to_json));
        ("exports", Json.array (List.map program.exports ~fn:Export.to_json));
      ]
end
