open Std
open Std.Data
module Core = RamlCore.CoreIR

module Binder: sig
  type t = {
    binding_id: Core.Binding_id.t;
    name: string;
  }
  val make: ?name:string -> Core.Binding_id.t -> t

  val generated: namespace:string list -> name:string -> t

  val entity_id: t -> Core.Entity_id.t

  val rename: t -> string -> t

  val to_json: t -> Json.t
end

type import_requirement = {
  from: string;
  imported: string option;
  local: Binder.t;
  namespace: bool;
}
type runtime_helper = {
  module_name: string;
  symbol: string;
  local: Binder.t;
}
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
  params: Binder.t list;
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
  target: Core.Entity_id.t;
  value: expr;
}

and expr =
  | Literal of literal
  | Identifier of Core.Entity_id.t
  | Imported of import_requirement
  | Runtime_helper of runtime_helper
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
  binder: Binder.t;
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
module Imports: sig
  type t = import_requirement = {
    from: string;
    imported: string option;
    local: Binder.t;
    namespace: bool;
  }
  type requirement = t
  val make: from:string -> ?imported:string -> local:Binder.t -> unit -> t

  val namespace: from:string -> local:Binder.t -> unit -> t

  val local: t -> Binder.t

  val equal: t -> t -> bool

  val to_json: t -> Json.t
end

module Runtime: sig
  type helper = runtime_helper = {
    module_name: string;
    symbol: string;
    local: Binder.t;
  }
  type t = helper
  val module_name: string

  val make: module_name:string -> symbol:string -> ?local:Binder.t -> unit -> helper

  val call_primitive: unit -> helper

  val make_curried: unit -> helper

  val print_endline: unit -> helper

  val print_newline: unit -> helper

  val print_int: unit -> helper

  val print_string: unit -> helper

  val print_char: unit -> helper

  val helper_for_direct_callee: string -> helper option

  val to_import: helper -> Imports.requirement

  val to_json: helper -> Json.t
end

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
    params: Binder.t list;
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
    target: Core.Entity_id.t;
    value: expr;
  }
  type t = expr =
    | Literal of Literal.t
    | Identifier of Core.Entity_id.t
    | Imported of Imports.requirement
    | Runtime_helper of Runtime.t
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
    binder: Binder.t;
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

module Export: sig
  type t = {
    name: string;
    local: Core.Entity_id.t;
  }
  val to_json: t -> Json.t
end

module Program: sig
  type t = {
    module_name: string;
    imports: Imports.requirement list;
    body: Statement.t list;
    exports: Export.t list;
  }
  val empty: module_name:string -> t

  val to_json: t -> Json.t
end
