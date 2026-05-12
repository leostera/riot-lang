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

module Expr = struct
  type callee =
    | Direct of string
    | Indirect of t

  and call = {
    callee: callee;
    arguments: t list;
  }

  and if_then_else = {
    condition: t;
    then_: t;
    else_: t;
  }

  and binding = {
    name: string;
    expr: t;
  }

  and let_ = {
    bindings: binding list;
    body: t;
  }

  and t =
    | Literal of Literal.t
    | Symbol of string
    | Symbol_address of string
    | Call of call
    | If_then_else of if_then_else
    | Let of let_

  let rec callee_to_json = fun callee ->
    match callee with
    | Direct name -> Json.obj [ ("kind", Json.string "direct"); ("name", Json.string name); ]
    | Indirect expr -> Json.obj [ ("kind", Json.string "indirect"); ("expr", to_json expr); ]

  and call_to_json = fun call ->
    Json.obj
      [
        ("callee", callee_to_json call.callee);
        ("arguments", Json.array (List.map call.arguments ~fn:to_json));
      ]

  and if_then_else_to_json = fun if_then_else ->
    Json.obj
      [
        ("condition", to_json if_then_else.condition);
        ("then", to_json if_then_else.then_);
        ("else", to_json if_then_else.else_);
      ]

  and binding_to_json = fun binding ->
    Json.obj [ ("name", Json.string binding.name); ("expr", to_json binding.expr); ]

  and let_to_json = fun let_ ->
    Json.obj
      [
        ("bindings", Json.array (List.map let_.bindings ~fn:binding_to_json));
        ("body", to_json let_.body);
      ]

  and to_json = fun expr ->
    match expr with
    | Literal literal -> Json.obj
      [ ("kind", Json.string "literal"); ("literal", Literal.to_json literal); ]
    | Symbol name -> Json.obj [ ("kind", Json.string "symbol"); ("name", Json.string name); ]
    | Symbol_address name -> Json.obj
      [ ("kind", Json.string "symbol_address"); ("name", Json.string name); ]
    | Call call -> Json.obj [ ("kind", Json.string "call"); ("call", call_to_json call); ]
    | If_then_else if_then_else -> Json.obj
      [ ("kind", Json.string "if_then_else"); ("if_then_else", if_then_else_to_json if_then_else); ]
    | Let let_ -> Json.obj [ ("kind", Json.string "let"); ("let", let_to_json let_); ]
end

module Function = struct
  type t = {
    name: string;
    params: string list;
    body: Expr.t;
  }

  let to_json = fun function_ ->
    Json.obj
      [
        ("name", Json.string function_.name);
        ("params", Json.array (List.map function_.params ~fn:Json.string));
        ("body", Expr.to_json function_.body);
      ]
end

module Binding = struct
  type t = {
    name: string;
    expr: Expr.t;
  }

  let to_json = fun binding ->
    Json.obj [ ("name", Json.string binding.name); ("expr", Expr.to_json binding.expr); ]
end

module Import_requirement = struct
  type linkage =
    | Runtime
    | External

  type t = {
    symbol: string;
    linkage: linkage;
  }

  let linkage_to_json = fun linkage ->
    match linkage with
    | Runtime -> Json.string "runtime"
    | External -> Json.string "external"

  let to_json = fun requirement ->
    Json.obj
      [
        ("symbol", Json.string requirement.symbol);
        ("linkage", linkage_to_json requirement.linkage);
      ]
end

module Runtime_helper = struct
  type t = {
    name: string;
    symbol: string;
  }

  let to_json = fun helper ->
    Json.obj [ ("name", Json.string helper.name); ("symbol", Json.string helper.symbol); ]
end

module Entry_item = struct
  type t =
    | Binding of Binding.t
    | Eval of Expr.t

  let to_json = fun entry_item ->
    match entry_item with
    | Binding binding -> Json.obj
      [ ("kind", Json.string "binding"); ("binding", Binding.to_json binding); ]
    | Eval expr -> Json.obj [ ("kind", Json.string "eval"); ("expr", Expr.to_json expr); ]
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
    imports: Import_requirement.t list;
    runtime_helpers: Runtime_helper.t list;
    functions: Function.t list;
    entry: Entry_item.t list;
    exports: Export.t list;
  }

  let empty = fun ~module_name ->
    {
      module_name;
      imports = [];
      runtime_helpers = [];
      functions = [];
      entry = [];
      exports = [];
    }

  let to_json = fun program ->
    Json.obj
      [
        ("module_name", Json.string program.module_name);
        ("imports", Json.array (List.map program.imports ~fn:Import_requirement.to_json));
        ("runtime_helpers", Json.array (List.map program.runtime_helpers ~fn:Runtime_helper.to_json));
        ("functions", Json.array (List.map program.functions ~fn:Function.to_json));
        ("entry", Json.array (List.map program.entry ~fn:Entry_item.to_json));
        ("exports", Json.array (List.map program.exports ~fn:Export.to_json));
      ]
end
