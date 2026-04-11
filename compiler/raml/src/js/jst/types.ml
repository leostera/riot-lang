open Std
open Std.Data

type literal_number =
  | Int of int
  | Float of float

type literal =
  | Undefined
  | Null
  | Bool of bool
  | Number of literal_number
  | String of string

type expr_call = {
  callee: expr;
  arguments: expr list;
}

and expr_function = {
  params: string list;
  body: statement list;
}

and expr_member = {
  object_: expr;
  property: string;
}

and expr_conditional = {
  condition: expr;
  then_: expr;
  else_: expr;
}

and expr_assignment = {
  target: string;
  value: expr;
}

and expr =
  | Literal of literal
  | Identifier of string
  | Function of expr_function
  | Member of expr_member
  | Call of expr_call
  | Conditional of expr_conditional
  | Assignment of expr_assignment

and declaration_kind =
  | Const
  | Let
  | Var

and declaration = {
  kind: declaration_kind;
  name: string;
  init: expr option;
}

and statement_if = {
  condition: expr;
  then_: statement list;
  else_: statement list;
}

and statement =
  | Declaration of declaration
  | Block of statement list
  | Expression of expr
  | Return of expr
  | If of statement_if

type import = {
  from: string;
  default: string option;
  namespace: string option;
  names: import_named list;
}

and import_named = {
  imported: string;
  local: string option;
}

let literal_number_to_json = fun number ->
  match number with
  | Int value -> Json.obj [ ("kind", Json.string "int"); ("value", Json.int value); ]
  | Float value -> Json.obj [ ("kind", Json.string "float"); ("value", Json.float value); ]

let literal_to_json = fun literal ->
  match literal with
  | Undefined -> Json.obj [ ("kind", Json.string "undefined") ]
  | Null -> Json.obj [ ("kind", Json.string "null") ]
  | Bool value -> Json.obj [ ("kind", Json.string "bool"); ("value", Json.bool value); ]
  | Number number -> Json.obj
    [ ("kind", Json.string "number"); ("number", literal_number_to_json number); ]
  | String value -> Json.obj [ ("kind", Json.string "string"); ("value", Json.string value); ]

let declaration_kind_to_json = fun kind ->
  match kind with
  | Const -> Json.string "const"
  | Let -> Json.string "let"
  | Var -> Json.string "var"

let rec expr_call_to_json = fun call ->
  Json.obj
    [
      ("callee", expr_to_json call.callee);
      ("arguments", Json.array (List.map expr_to_json call.arguments));
    ]

and expr_function_to_json = fun function_ ->
  Json.obj
    [
      ("params", Json.array (List.map Json.string function_.params));
      ("body", Json.array (List.map statement_to_json function_.body));
    ]

and expr_member_to_json = fun member ->
  Json.obj [ ("object", expr_to_json member.object_); ("property", Json.string member.property); ]

and expr_conditional_to_json = fun conditional ->
  Json.obj
    [
      ("condition", expr_to_json conditional.condition);
      ("then", expr_to_json conditional.then_);
      ("else", expr_to_json conditional.else_);
    ]

and expr_assignment_to_json = fun assignment ->
  Json.obj [ ("target", Json.string assignment.target); ("value", expr_to_json assignment.value); ]

and expr_to_json = fun expr ->
  match expr with
  | Literal literal -> Json.obj
    [ ("kind", Json.string "literal"); ("literal", literal_to_json literal); ]
  | Identifier name -> Json.obj [ ("kind", Json.string "identifier"); ("name", Json.string name); ]
  | Function function_ -> Json.obj
    [ ("kind", Json.string "function"); ("function", expr_function_to_json function_); ]
  | Member member -> Json.obj
    [ ("kind", Json.string "member"); ("member", expr_member_to_json member); ]
  | Call call -> Json.obj [ ("kind", Json.string "call"); ("call", expr_call_to_json call); ]
  | Conditional conditional -> Json.obj
    [ ("kind", Json.string "conditional"); ("conditional", expr_conditional_to_json conditional); ]
  | Assignment assignment -> Json.obj
    [ ("kind", Json.string "assignment"); ("assignment", expr_assignment_to_json assignment); ]

and declaration_to_json = fun declaration ->
  Json.obj
    [
      ("kind", declaration_kind_to_json declaration.kind);
      ("name", Json.string declaration.name);
      ("init", Option.map expr_to_json declaration.init |> Option.unwrap_or ~default:Json.null);
    ]

and statement_if_to_json = fun (if_: statement_if) ->
  Json.obj
    [
      ("condition", expr_to_json if_.condition);
      ("then", Json.array (List.map statement_to_json if_.then_));
      ("else", Json.array (List.map statement_to_json if_.else_));
    ]

and statement_to_json = fun statement ->
  match statement with
  | Declaration declaration -> Json.obj
    [ ("kind", Json.string "declaration"); ("declaration", declaration_to_json declaration); ]
  | Block statements -> Json.obj
    [ ("kind", Json.string "block"); ("body", Json.array (List.map statement_to_json statements)); ]
  | Expression expr -> Json.obj
    [ ("kind", Json.string "expression"); ("expression", expr_to_json expr); ]
  | Return expr -> Json.obj [ ("kind", Json.string "return"); ("expression", expr_to_json expr); ]
  | If if_ -> Json.obj [ ("kind", Json.string "if"); ("if", statement_if_to_json if_); ]

let import_named_to_json = fun named ->
  Json.obj
    [
      ("imported", Json.string named.imported);
      ("local", Option.map Json.string named.local |> Option.unwrap_or ~default:Json.null);
    ]

let import_to_json = fun import ->
  let fields = [
    ("from", Json.string import.from);
    ("default", Option.map Json.string import.default |> Option.unwrap_or ~default:Json.null);
    ("names", Json.array (List.map import_named_to_json import.names));
  ] in
  match import.namespace with
  | None -> Json.obj fields
  | Some namespace -> Json.obj (fields @ [ ("namespace", Json.string namespace); ])

module Literal = struct
  type number = literal_number =
    | Int of int
    | Float of float

  type t = literal =
    | Undefined
    | Null
    | Bool of bool
    | Number of number
    | String of string

  let number_to_json = literal_number_to_json

  let to_json = literal_to_json
end

module Expr = struct
  type call = expr_call = {
    callee: expr;
    arguments: expr list;
  }

  type function_ = expr_function = {
    params: string list;
    body: statement list;
  }

  type member = expr_member = {
    object_: expr;
    property: string;
  }

  type conditional = expr_conditional = {
    condition: expr;
    then_: expr;
    else_: expr;
  }

  type assignment = expr_assignment = {
    target: string;
    value: expr;
  }

  type t = expr =
    | Literal of Literal.t
    | Identifier of string
    | Function of function_
    | Member of member
    | Call of call
    | Conditional of conditional
    | Assignment of assignment

  let call_to_json = expr_call_to_json

  let function_to_json = expr_function_to_json

  let member_to_json = expr_member_to_json

  let conditional_to_json = expr_conditional_to_json

  let assignment_to_json = expr_assignment_to_json

  let to_json = expr_to_json
end

module Declaration = struct
  type kind = declaration_kind =
    | Const
    | Let
    | Var

  type t = declaration = {
    kind: kind;
    name: string;
    init: expr option;
  }

  let kind_to_json = declaration_kind_to_json

  let to_json = declaration_to_json
end

module Statement = struct
  type if_ = statement_if = {
    condition: expr;
    then_: statement list;
    else_: statement list;
  }

  type t = statement =
    | Declaration of declaration
    | Block of statement list
    | Expression of expr
    | Return of expr
    | If of if_

  let if_to_json = statement_if_to_json

  let to_json = statement_to_json
end

module Import = struct
  type named = import_named = {
    imported: string;
    local: string option;
  }

  type t = import = {
    from: string;
    default: string option;
    namespace: string option;
    names: named list;
  }

  let named_to_json = import_named_to_json

  let to_json = import_to_json
end

module Export = struct
  type t = {
    name: string;
    local: string;
  }

  let to_json = fun export ->
    Json.obj [ ("name", Json.string export.name); ("local", Json.string export.local); ]
end

module Module_item = struct
  type t =
    | Import of Import.t
    | Statement of Statement.t
    | Export of Export.t list

  let to_json = fun item ->
    match item with
    | Import import -> Json.obj
      [ ("kind", Json.string "import"); ("import", Import.to_json import); ]
    | Statement statement -> Json.obj
      [ ("kind", Json.string "statement"); ("statement", Statement.to_json statement); ]
    | Export exports -> Json.obj
      [ ("kind", Json.string "export"); ("exports", Json.array (List.map Export.to_json exports)); ]
end

module Program = struct
  type t = {
    module_name: string;
    items: Module_item.t list;
  }

  let empty = fun ~module_name -> { module_name; items = [] }

  let to_json = fun program ->
    Json.obj
      [
        ("module_name", Json.string program.module_name);
        ("items", Json.array (List.map Module_item.to_json program.items));
      ]
end
