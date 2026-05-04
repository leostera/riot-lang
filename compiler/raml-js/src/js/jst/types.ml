open Std
open Std.Data
module Core = Raml_core.Core_ir

module Binder = struct
  type t = {
    binding_id: Core.Binding_id.t;
    name: string;
  }

  let make = fun ?name binding_id ->
    { binding_id; name = Option.unwrap_or ~default:(Core.Binding_id.name binding_id) name }

  let entity_id = fun binder -> Core.Entity_id.from_binding_id binder.binding_id

  let rename = fun binder name -> { binder with name }

  let to_json = fun binder ->
    Json.obj
      [
        ("binding_id", Core.Binding_id.to_json binder.binding_id);
        ("name", Json.string binder.name);
      ]
end

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

type module_ref = {
  kind: Jir.Types.Modules.kind;
  unit_name: string;
  import_path: string;
  namespace: string list;
}

type import = {
  from: module_ref;
  default: Binder.t option;
  namespace: Binder.t option;
  names: import_named list;
}

and import_named = {
  imported: string;
  local: Binder.t;
}

let literal_number_to_json = fun number ->
  match number with
  | Int value -> Json.obj [ ("kind", Json.string "int"); ("value", Json.int value) ]
  | Float value -> Json.obj [ ("kind", Json.string "float"); ("value", Json.float value) ]

let literal_to_json = fun literal ->
  match literal with
  | Undefined -> Json.obj [ ("kind", Json.string "undefined") ]
  | Null -> Json.obj [ ("kind", Json.string "null") ]
  | Bool value -> Json.obj [ ("kind", Json.string "bool"); ("value", Json.bool value) ]
  | Number number -> Json.obj
    [ ("kind", Json.string "number"); ("number", literal_number_to_json number) ]
  | String value -> Json.obj [ ("kind", Json.string "string"); ("value", Json.string value) ]

let unary_operator_to_json = fun operator ->
  match operator with
  | Not -> Json.string "not"
  | Negate -> Json.string "negate"

let binary_operator_to_json = fun operator ->
  match operator with
  | Add -> Json.string "add"
  | Subtract -> Json.string "subtract"
  | Multiply -> Json.string "multiply"
  | Divide -> Json.string "divide"
  | Modulo -> Json.string "modulo"
  | Equal -> Json.string "equal"
  | Not_equal -> Json.string "not_equal"
  | Less_than -> Json.string "less_than"
  | Less_or_equal -> Json.string "less_or_equal"
  | Greater_than -> Json.string "greater_than"
  | Greater_or_equal -> Json.string "greater_or_equal"

let declaration_kind_to_json = fun kind ->
  match kind with
  | Const -> Json.string "const"
  | Let -> Json.string "let"
  | Var -> Json.string "var"

let rec expr_call_to_json = fun (call: expr_call) ->
  Json.obj
    [
      ("callee", expr_to_json call.callee);
      ("arguments", Json.array (List.map call.arguments ~fn:expr_to_json));
    ]

and expr_global_to_json = fun (global: expr_global) -> Json.obj [ ("name", Json.string global.name) ]

and expr_unary_to_json = fun (unary: expr_unary) ->
  Json.obj
    [ ("operator", unary_operator_to_json unary.operator); ("operand", expr_to_json unary.operand); ]

and expr_binary_to_json = fun (binary: expr_binary) ->
  Json.obj
    [
      ("operator", binary_operator_to_json binary.operator);
      ("left", expr_to_json binary.left);
      ("right", expr_to_json binary.right);
    ]

and expr_array_element_to_json = fun element ->
  match element with
  | Item expr -> Json.obj [ ("kind", Json.string "item"); ("expr", expr_to_json expr) ]
  | Spread expr -> Json.obj [ ("kind", Json.string "spread"); ("expr", expr_to_json expr) ]

and expr_array_to_json = fun array -> Json.array (List.map array ~fn:expr_array_element_to_json)

and expr_object_field_to_json = fun (field: expr_object_field) ->
  Json.obj [ ("name", Json.string field.name); ("value", expr_to_json field.value); ]

and expr_object_to_json = fun object_ -> Json.array (List.map object_ ~fn:expr_object_field_to_json)

and expr_function_to_json = fun (function_: expr_function) ->
  Json.obj
    [
      ("params", Json.array (List.map function_.params ~fn:Binder.to_json));
      ("body", Json.array (List.map function_.body ~fn:statement_to_json));
    ]

and expr_member_to_json = fun (member: expr_member) ->
  Json.obj [ ("object", expr_to_json member.object_); ("property", Json.string member.property) ]

and expr_index_to_json = fun (index: expr_index) ->
  Json.obj [ ("object", expr_to_json index.object_); ("index", expr_to_json index.index); ]

and expr_conditional_to_json = fun (conditional: expr_conditional) ->
  Json.obj
    [
      ("condition", expr_to_json conditional.condition);
      ("then", expr_to_json conditional.then_);
      ("else", expr_to_json conditional.else_);
    ]

and expr_assignment_to_json = fun (assignment: expr_assignment) ->
  Json.obj
    [
      ("target", Core.Entity_id.to_json assignment.target);
      ("value", expr_to_json assignment.value);
    ]

and expr_to_json = fun expr ->
  match expr with
  | Literal literal -> Json.obj
    [ ("kind", Json.string "literal"); ("literal", literal_to_json literal) ]
  | Global global -> Json.obj
    [ ("kind", Json.string "global"); ("global", expr_global_to_json global) ]
  | Identifier entity_id -> Json.obj
    [ ("kind", Json.string "identifier"); ("entity_id", Core.Entity_id.to_json entity_id) ]
  | Unary unary -> Json.obj [ ("kind", Json.string "unary"); ("unary", expr_unary_to_json unary) ]
  | Binary binary -> Json.obj
    [ ("kind", Json.string "binary"); ("binary", expr_binary_to_json binary) ]
  | Array array -> Json.obj [ ("kind", Json.string "array"); ("array", expr_array_to_json array) ]
  | Object object_ -> Json.obj
    [ ("kind", Json.string "object"); ("object", expr_object_to_json object_) ]
  | Function function_ -> Json.obj
    [ ("kind", Json.string "function"); ("function", expr_function_to_json function_) ]
  | Member member -> Json.obj
    [ ("kind", Json.string "member"); ("member", expr_member_to_json member) ]
  | Index index -> Json.obj [ ("kind", Json.string "index"); ("index", expr_index_to_json index) ]
  | Call call -> Json.obj [ ("kind", Json.string "call"); ("call", expr_call_to_json call) ]
  | Conditional conditional -> Json.obj
    [ ("kind", Json.string "conditional"); ("conditional", expr_conditional_to_json conditional) ]
  | Assignment assignment -> Json.obj
    [ ("kind", Json.string "assignment"); ("assignment", expr_assignment_to_json assignment) ]

and declaration_to_json = fun (declaration: declaration) ->
  Json.obj
    [
      ("kind", declaration_kind_to_json declaration.kind);
      ("binder", Binder.to_json declaration.binder);
      ("init", Option.map declaration.init ~fn:expr_to_json |> Option.unwrap_or ~default:Json.null);
    ]

and statement_if_to_json = fun (if_: statement_if) ->
  Json.obj
    [
      ("condition", expr_to_json if_.condition);
      ("then", Json.array (List.map if_.then_ ~fn:statement_to_json));
      ("else", Json.array (List.map if_.else_ ~fn:statement_to_json));
    ]

and statement_to_json = fun statement ->
  match statement with
  | Declaration declaration -> Json.obj
    [ ("kind", Json.string "declaration"); ("declaration", declaration_to_json declaration) ]
  | Block statements -> Json.obj
    [ ("kind", Json.string "block"); ("body", Json.array (List.map statements ~fn:statement_to_json)) ]
  | Expression expr -> Json.obj
    [ ("kind", Json.string "expression"); ("expression", expr_to_json expr) ]
  | Return expr -> Json.obj [ ("kind", Json.string "return"); ("expression", expr_to_json expr) ]
  | If if_ -> Json.obj [ ("kind", Json.string "if"); ("if", statement_if_to_json if_) ]

let import_named_to_json = fun named ->
  Json.obj [ ("imported", Json.string named.imported); ("local", Binder.to_json named.local); ]

let module_ref_to_json = fun (module_ref: module_ref) ->
  Json.obj
    [
      ("kind", Jir.Types.Modules.kind_to_json module_ref.kind);
      ("unit_name", Json.string module_ref.unit_name);
      ("import_path", Json.string module_ref.import_path);
      ("namespace", Json.array (List.map module_ref.namespace ~fn:Json.string));
    ]

let import_to_json = fun (import: import) ->
  let fields = [
    ("from", module_ref_to_json import.from);
    ("default", Option.map import.default ~fn:Binder.to_json |> Option.unwrap_or ~default:Json.null);
    ("names", Json.array (List.map import.names ~fn:import_named_to_json));
  ] in
  match import.namespace with
  | None -> Json.obj fields
  | Some namespace -> Json.obj (fields @ [ ("namespace", Binder.to_json namespace) ])

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

module Operator = struct
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

  let unary_to_json = unary_operator_to_json

  let binary_to_json = binary_operator_to_json
end

module Expr = struct
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

  let global_to_json = expr_global_to_json

  let unary_to_json = expr_unary_to_json

  let binary_to_json = expr_binary_to_json

  let array_element_to_json = expr_array_element_to_json

  let array_to_json = expr_array_to_json

  let object_field_to_json = expr_object_field_to_json

  let object_to_json = expr_object_to_json

  let call_to_json = expr_call_to_json

  let function_to_json = expr_function_to_json

  let member_to_json = expr_member_to_json

  let index_to_json = expr_index_to_json

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
    binder: Binder.t;
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
    local: Binder.t;
  }

  type t = import = {
    from: module_ref;
    default: Binder.t option;
    namespace: Binder.t option;
    names: named list;
  }

  let module_ref_to_json = module_ref_to_json

  let named_to_json = import_named_to_json

  let to_json = import_to_json
end

module Export = struct
  type t = {
    name: string;
    local: Core.Entity_id.t;
  }

  let to_json = fun export ->
    Json.obj [ ("name", Json.string export.name); ("local", Core.Entity_id.to_json export.local); ]
end

module Module_item = struct
  type t =
    | Import of Import.t
    | Statement of Statement.t
    | Export of Export.t list

  let to_json = fun item ->
    match item with
    | Import import -> Json.obj [ ("kind", Json.string "import"); ("import", Import.to_json import) ]
    | Statement statement -> Json.obj
      [ ("kind", Json.string "statement"); ("statement", Statement.to_json statement) ]
    | Export exports -> Json.obj
      [ ("kind", Json.string "export"); ("exports", Json.array (List.map exports ~fn:Export.to_json)) ]
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
        ("items", Json.array (List.map program.items ~fn:Module_item.to_json));
      ]
end
