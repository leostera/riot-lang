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
module Literal: sig
  type number = literal_number =
    | Int of int
    | Float of float
  type t = literal =
    | Undefined
    | Null
    | Bool of bool
    | Number of number
    | String of string
  val number_to_json: number -> Json.t

  val to_json: t -> Json.t
end

module Expr: sig
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
  val call_to_json: call -> Json.t

  val function_to_json: function_ -> Json.t

  val member_to_json: member -> Json.t

  val conditional_to_json: conditional -> Json.t

  val assignment_to_json: assignment -> Json.t

  val to_json: t -> Json.t
end

module Declaration: sig
  type kind = declaration_kind =
    | Const
    | Let
    | Var
  type t = declaration = {
    kind: kind;
    name: string;
    init: expr option;
  }
  val kind_to_json: kind -> Json.t

  val to_json: t -> Json.t
end

module Statement: sig
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
  val if_to_json: if_ -> Json.t

  val to_json: t -> Json.t
end

module Import: sig
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
  val named_to_json: named -> Json.t

  val to_json: t -> Json.t
end

module Export: sig
  type t = {
    name: string;
    local: string;
  }
  val to_json: t -> Json.t
end

module Module_item: sig
  type t =
    | Import of Import.t
    | Statement of Statement.t
    | Export of Export.t list
  val to_json: t -> Json.t
end

module Program: sig
  type t = {
    module_name: string;
    items: Module_item.t list;
  }
  val empty: module_name:string -> t

  val to_json: t -> Json.t
end
