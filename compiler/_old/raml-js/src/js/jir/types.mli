open Std
open Std.Data

module Core = Raml_core.Core_ir

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

module Modules: sig
  type kind =
    | Relative_unit
    | Runtime
  type t = {
    kind: kind;
    unit_name: string;
    import_path: string;
    namespace: string list;
  }
  val sibling_unit: string -> t

  val runtime: string -> t

  val namespace_binder: t -> Binder.t

  val import_path: t -> string

  val namespace_segments: t -> string list

  val compare: t -> t -> Std.Order.t

  val equal: t -> t -> bool

  val kind_to_json: kind -> Json.t

  val to_json: t -> Json.t
end

type import_requirement = {
  from: Modules.t;
  imported: string option;
  local: Binder.t;
  namespace: bool;
}
type runtime_helper = {
  module_ref: Modules.t;
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
type unary_operator =
  | Not
  | Negate
type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | Not_equal
  | Less_than
  | Less_or_equal
  | Greater_than
  | Greater_or_equal
type expr =
  | Literal of literal
  | Global of expr_global
  | Identifier of Core.Entity_id.t
  | Imported of import_requirement
  | Runtime_helper of runtime_helper
  | Unary of expr_unary
  | Binary of expr_binary
  | Array of expr_array
  | Object of expr_object
  | Function of expr_function
  | Member of expr_member
  | Index of expr_index
  | Call of expr_call
  | Conditional of expr_conditional
  | Assignment of expr_assignment

and expr_global = {
  name: string;
}

and expr_unary = {
  operator: unary_operator;
  operand: expr;
}

and expr_binary = {
  operator: binary_operator;
  left: expr;
  right: expr;
}

and expr_array_element =
  | Item of expr
  | Spread of expr

and expr_array = expr_array_element list

and expr_object_field = {
  name: string;
  value: expr;
}

and expr_object = expr_object_field list

and expr_call = {
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

and expr_index = {
  object_: expr;
  index: expr;
}

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
    from: Modules.t;
    imported: string option;
    local: Binder.t;
    namespace: bool;
  }
  type requirement = t
  val make: from:Modules.t -> ?imported:string -> local:Binder.t -> unit -> t

  val namespace: from:Modules.t -> local:Binder.t -> unit -> t

  val local: t -> Binder.t

  val equal: t -> t -> bool

  val to_json: t -> Json.t
end

module Runtime: sig
  type helper = runtime_helper = {
    module_ref: Modules.t;
    symbol: string;
    local: Binder.t;
  }
  type t = helper
  val module_ref: Modules.t

  val make: module_ref:Modules.t -> symbol:string -> ?local:Binder.t -> unit -> helper

  val call_primitive: unit -> helper

  val make_curried: unit -> helper

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

module Operator: sig
  type unary = unary_operator =
    | Not
    | Negate
  type binary = binary_operator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | Not_equal
    | Less_than
    | Less_or_equal
    | Greater_than
    | Greater_or_equal
  val unary_to_json: unary -> Json.t

  val binary_to_json: binary -> Json.t
end

module Expr: sig
  type global = expr_global = {
    name: string;
  }
  type unary = expr_unary = {
    operator: unary_operator;
    operand: expr;
  }
  type binary = expr_binary = {
    operator: binary_operator;
    left: expr;
    right: expr;
  }
  type array_element = expr_array_element =
    | Item of expr
    | Spread of expr
  type array = expr_array
  type object_field = expr_object_field = {
    name: string;
    value: expr;
  }
  type object_ = expr_object
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
  type index = expr_index = {
    object_: expr;
    index: expr;
  }
  type t = expr =
    | Literal of Literal.t
    | Global of global
    | Identifier of Core.Entity_id.t
    | Imported of Imports.requirement
    | Runtime_helper of Runtime.t
    | Unary of unary
    | Binary of binary
    | Array of array
    | Object of object_
    | Function of function_
    | Member of member
    | Index of index
    | Call of call
    | Conditional of conditional
    | Assignment of assignment
  val global_to_json: global -> Json.t

  val unary_to_json: unary -> Json.t

  val binary_to_json: binary -> Json.t

  val array_element_to_json: array_element -> Json.t

  val array_to_json: array -> Json.t

  val object_field_to_json: object_field -> Json.t

  val object_to_json: object_ -> Json.t

  val call_to_json: call -> Json.t

  val function_to_json: function_ -> Json.t

  val member_to_json: member -> Json.t

  val index_to_json: index -> Json.t

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
