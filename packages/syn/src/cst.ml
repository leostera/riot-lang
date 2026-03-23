open Std
open Std.Collections

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type green_node = (Syntax_kind.t, string) Ceibo.Green.node

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

module Token = struct
  type t = { syntax_token : syntax_token }

  let syntax_token token = token.syntax_token
  let text token = Ceibo.Red.SyntaxToken.text token.syntax_token
  let span token = Ceibo.Red.SyntaxToken.span token.syntax_token
end

module Ident = struct
  type t =
    | Ident of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Qualified of {
        syntax_node : syntax_node;
        prefix : t;
        dot_token : Token.t;
        name_token : Token.t;
      }

  let syntax_node = function
    | Ident { syntax_node; _ } -> syntax_node
    | Qualified { syntax_node; _ } -> syntax_node

  let rec segments = function
    | Ident { name_token; _ } -> [ name_token ]
    | Qualified { prefix; name_token; _ } -> segments prefix @ [ name_token ]

  let last_segment = function
    | Ident { name_token; _ } -> Some name_token
    | Qualified { name_token; _ } -> Some name_token

  let name path =
    match last_segment path with
    | Some segment -> Some (Token.text segment)
    | None -> None
end

module ModulePath = Ident

type attribute = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
}

type extension = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
}

type object_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
}

and type_binder =
  | Quoted of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
  | Bare of {
      name_token : Token.t;
    }

and record_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
  is_mutable : bool;
}

and poly_variant_tag = {
  syntax_node : syntax_node;
  tag_name : Token.t;
  payload_type : core_type option;
}

and poly_variant_bound =
  | Exact
  | UpperBound of {
      marker_token : Token.t;
    }
  | LowerBound of {
      marker_token : Token.t;
    }

and row_field =
  | Tag of poly_variant_tag
  | Inherit of {
      syntax_node : syntax_node;
      type_ : core_type;
    }

and poly_variant = {
  syntax_node : syntax_node;
  kind : poly_variant_bound;
  fields : row_field list;
}

and module_type_constraint = {
  syntax_node : syntax_node;
  type_name : Token.t;
  replacement_type : core_type;
  is_destructive : bool;
}

and functor_parameter = {
  syntax_node : syntax_node;
  name_token : Token.t;
  module_type : module_type;
}

and local_open_core_type = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  type_ : core_type;
}

and core_type =
  | Wildcard of {
      syntax_node : syntax_node;
      wildcard_token : Token.t;
    }
  | Var of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
  | Constr of {
      syntax_node : syntax_node;
      constructor_path : Ident.t;
      arguments : core_type list;
    }
  | Class of {
      syntax_node : syntax_node;
      hash_token : Token.t;
      class_path : Ident.t;
      arguments : core_type list;
    }
  | Alias of {
      syntax_node : syntax_node;
      type_ : core_type;
      name_token : Token.t;
    }
  | Attribute of {
      syntax_node : syntax_node;
      type_ : core_type;
      attribute : attribute;
    }
  | Extension of extension
  | Poly of {
      syntax_node : syntax_node;
      binders : type_binder list;
      body : core_type;
    }
  | Arrow of {
      syntax_node : syntax_node;
      parameter_type : core_type;
      result_type : core_type;
    }
  | Tuple of {
      syntax_node : syntax_node;
      elements : core_type list;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : core_type;
    }
  | LocalOpen of local_open_core_type
  | PolyVariant of poly_variant
  | Record of {
      syntax_node : syntax_node;
      fields : record_type_field list;
    }
  | FirstClassModule of {
      syntax_node : syntax_node;
      module_type : module_type;
    }
  | Object of {
      syntax_node : syntax_node;
      fields : object_type_field list;
    }

and module_type =
  | Path of Ident.t
  | TypeOf of {
      syntax_node : syntax_node;
      module_path : Ident.t;
    }
  | Signature of {
      syntax_node : syntax_node;
      signature_syntax_node : syntax_node;
    }
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      result : module_type;
    }
  | With of {
      syntax_node : syntax_node;
      base : module_type;
      constraints : module_type_constraint list;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_type;
    }
  | Attribute of {
      syntax_node : syntax_node;
      module_type : module_type;
      attribute : attribute;
    }
  | Extension of extension

module CoreType = struct
  type t = core_type =
    | Wildcard of {
        syntax_node : syntax_node;
        wildcard_token : Token.t;
      }
    | Var of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Constr of {
        syntax_node : syntax_node;
        constructor_path : Ident.t;
        arguments : core_type list;
      }
    | Class of {
        syntax_node : syntax_node;
        hash_token : Token.t;
        class_path : Ident.t;
        arguments : core_type list;
      }
    | Alias of {
        syntax_node : syntax_node;
        type_ : core_type;
        name_token : Token.t;
      }
    | Attribute of {
        syntax_node : syntax_node;
        type_ : core_type;
        attribute : attribute;
      }
    | Extension of extension
    | Poly of {
        syntax_node : syntax_node;
        binders : type_binder list;
        body : core_type;
      }
    | Arrow of {
        syntax_node : syntax_node;
        parameter_type : core_type;
        result_type : core_type;
      }
    | Tuple of {
        syntax_node : syntax_node;
        elements : core_type list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : core_type;
      }
    | LocalOpen of local_open_core_type
    | PolyVariant of poly_variant
    | Record of {
        syntax_node : syntax_node;
        fields : record_type_field list;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }

  let syntax_node = function
    | Wildcard { syntax_node; _ }
    | Var { syntax_node; _ }
    | Constr { syntax_node; _ }
    | Class { syntax_node; _ }
    | Alias { syntax_node; _ }
    | Attribute { syntax_node; _ }
    | Extension { syntax_node; _ }
    | Poly { syntax_node; _ }
    | Arrow { syntax_node; _ }
    | Tuple { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | LocalOpen { syntax_node; _ }
    | Record { syntax_node; _ }
    | FirstClassModule { syntax_node; _ }
    | Object { syntax_node; _ } ->
        syntax_node
    | PolyVariant poly_variant ->
        poly_variant.syntax_node
end

module ModuleTypeConstraint = struct
  type t = module_type_constraint = {
    syntax_node : syntax_node;
    type_name : Token.t;
    replacement_type : core_type;
    is_destructive : bool;
  }
end

module FunctorParameter = struct
  type t = functor_parameter = {
    syntax_node : syntax_node;
    name_token : Token.t;
    module_type : module_type;
  }
end

module ModuleType = struct
  type t = module_type =
    | Path of Ident.t
    | TypeOf of {
        syntax_node : syntax_node;
        module_path : Ident.t;
      }
    | Signature of {
        syntax_node : syntax_node;
        signature_syntax_node : syntax_node;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        result : module_type;
      }
    | With of {
        syntax_node : syntax_node;
        base : module_type;
        constraints : module_type_constraint list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_type;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_type : module_type;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | TypeOf { syntax_node; _ }
    | Signature { syntax_node; _ }
    | Functor { syntax_node; _ }
    | With { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

module PatternLiteral = struct
  type t =
    | String of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Int of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Float of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Char of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Bool of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Unit of { syntax_node : syntax_node }
end

module TypeBinder = struct
  type t = type_binder =
    | Quoted of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Bare of {
        name_token : Token.t;
      }

  let name_token = function
    | Quoted { name_token; _ } | Bare { name_token } ->
        name_token

  let name binder = Token.text (name_token binder)

  let text = function
    | Quoted { name_token; _ } ->
        "'" ^ Token.text name_token
    | Bare { name_token } ->
        Token.text name_token

  let is_quoted = function
    | Quoted _ ->
        true
    | Bare _ ->
        false
end

type pattern_literal = PatternLiteral.t

type pattern =
  | Identifier of identifier_pattern
  | Wildcard of wildcard_pattern
  | Attribute of attributed_pattern
  | Extension of extension
  | Literal of pattern_literal
  | Lazy of lazy_pattern
  | Exception of exception_pattern
  | Range of range_pattern
  | Operator of operator_pattern
  | FirstClassModule of first_class_module_pattern
  | PolyVariant of poly_variant_pattern
  | PolyVariantInherit of poly_variant_inherit_pattern
  | Constructor of constructor_pattern
  | Tuple of tuple_pattern
  | List of list_pattern
  | Array of array_pattern
  | Record of record_pattern
  | Cons of cons_pattern
  | Or of or_pattern
  | Alias of alias_pattern
  | Typed of typed_pattern
  | Effect of effect_pattern
  | LocalOpen of local_open_pattern
  | Parenthesized of parenthesized_pattern

and identifier_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

and attributed_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  attribute : attribute;
}

and wildcard_pattern = {
  syntax_node : syntax_node;
}

and lazy_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
}

and exception_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
}

and range_pattern = {
  syntax_node : syntax_node;
  lower_token : Token.t;
  upper_token : Token.t;
}

and operator_pattern = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
}

and first_class_module_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
  module_type : module_type option;
}

and poly_variant_pattern = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : pattern option;
}

and poly_variant_inherit_pattern = {
  syntax_node : syntax_node;
  type_path : Ident.t;
}

and constructor_pattern = {
  syntax_node : syntax_node;
  constructor_path : Ident.t;
  arguments : pattern list;
}

and tuple_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

and list_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

and array_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

and record_pattern = {
  syntax_node : syntax_node;
  fields : record_pattern_field list;
}

and record_pattern_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  pattern : pattern option;
}

and cons_pattern = {
  syntax_node : syntax_node;
  head : pattern;
  tail : pattern;
}

and or_pattern = {
  syntax_node : syntax_node;
  alternatives : pattern list;
}

and alias_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  name_token : Token.t;
}

and typed_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  type_ : core_type;
}

and effect_pattern = {
  syntax_node : syntax_node;
  effect_pattern : pattern;
  continuation : pattern;
}

and local_open_pattern = {
  syntax_node : syntax_node;
  module_path : ModulePath.t;
  pattern : pattern;
}

and parenthesized_pattern = {
  syntax_node : syntax_node;
  inner : pattern;
}

type positional_parameter = {
  syntax_node : syntax_node;
  name_token : Token.t option;
}

type labeled_parameter = {
  syntax_node : syntax_node;
  label_token : Token.t;
  binding_name_token : Token.t option;
}

type optional_parameter = {
  syntax_node : syntax_node;
  label_token : Token.t;
  binding_name_token : Token.t option;
  has_default : bool;
}

type locally_abstract_type_parameter = {
  syntax_node : syntax_node;
  binders : type_binder list;
}

type parameter =
  | Positional of positional_parameter
  | Labeled of labeled_parameter
  | Optional of optional_parameter
  | LocallyAbstract of locally_abstract_type_parameter

module Parameter = struct
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of locally_abstract_type_parameter

  let syntax_node = function
    | Positional param -> param.syntax_node
    | Labeled param -> param.syntax_node
    | Optional param -> param.syntax_node
    | LocallyAbstract param -> param.syntax_node

  let name_token = function
    | Positional param -> param.name_token
    | Labeled param -> Some param.label_token
    | Optional param -> Some param.label_token
    | LocallyAbstract _ ->
        None

  let name param =
    match name_token param with
    | Some token -> Some (Token.text token)
    | None -> None

  let is_named = function
    | Labeled _ | Optional _ -> true
    | Positional _ | LocallyAbstract _ ->
        false

  let has_default = function
    | Optional param -> param.has_default
    | Positional _ | Labeled _ | LocallyAbstract _ ->
        false
end

module Literal = struct
  type t =
    | String of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Int of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Float of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Char of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Bool of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
    | Unit of { syntax_node : syntax_node }
end

type literal = Literal.t

type exception_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

type expression =
  | Path of path_expression
  | Operator of operator_expression
  | Literal of literal
  | Unreachable of unreachable_expression
  | Attribute of attribute
  | Extension of extension
  | Object of object_expression
  | PolyVariant of poly_variant_expression
  | FirstClassModule of first_class_module_expression
  | LetModule of let_module_expression
  | LetException of let_exception_expression
  | Assert of assert_expression
  | Lazy of lazy_expression
  | While of while_expression
  | For of for_expression
  | Apply of apply_expression
  | MethodCall of method_call_expression
  | New of new_expression
  | Prefix of prefix_expression
  | FieldAccess of field_access_expression
  | Index of index_expression
  | ObjectUpdate of object_update_expression
  | InstanceVariableAssign of instance_variable_assign_expression
  | Assign of assign_expression
  | Infix of infix_expression
  | Typed of typed_expression
  | Polymorphic of polymorphic_expression
  | Coerce of coerce_expression
  | Sequence of sequence_expression
  | Tuple of tuple_expression
  | List of list_expression
  | Array of array_expression
  | Record of record_expression
  | LocalOpen of local_open_expression
  | Fun of fun_expression
  | Function of function_expression
  | LetOperator of let_operator_expression
  | Let of let_expression
  | Match of match_expression
  | Try of try_expression
  | If of if_expression
  | Parenthesized of parenthesized_expression

and path_expression = {
  syntax_node : syntax_node;
  path : Ident.t;
}

and operator_expression = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
}

and unreachable_expression = {
  syntax_node : syntax_node;
  dot_token : Token.t;
}

and object_expression = {
  syntax_node : syntax_node;
  self_pattern : pattern option;
  members : object_member list;
}

and object_member =
  | Method of object_method
  | Value of object_value
  | Inherit of object_inherit
  | Initializer of object_initializer

and object_method = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  body : expression option;
  type_ : core_type option;
  is_private : bool;
  is_virtual : bool;
  is_override : bool;
}

and object_value = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  value : expression option;
  type_ : core_type option;
  is_mutable : bool;
  is_virtual : bool;
  is_override : bool;
}

and object_inherit = {
  syntax_node : syntax_node;
  attributes : attribute list;
  expression : expression;
}

and object_initializer = {
  syntax_node : syntax_node;
  body : expression option;
}

and poly_variant_expression = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : expression option;
}

and first_class_module_expression = {
  syntax_node : syntax_node;
  module_expression : module_expression;
  module_type : module_type option;
}

and let_module_expression = {
  syntax_node : syntax_node;
  module_name_token : Token.t;
  module_expression : module_expression;
  body : expression;
}

and let_exception_expression = {
  syntax_node : syntax_node;
  exception_declaration : exception_declaration;
  body : expression;
}

and assert_expression = {
  syntax_node : syntax_node;
  asserted : expression;
}

and lazy_expression = {
  syntax_node : syntax_node;
  body : expression;
}

and while_expression = {
  syntax_node : syntax_node;
  condition : expression;
  body : expression;
}

and for_expression = {
  syntax_node : syntax_node;
  iterator_token : Token.t;
  start_expr : expression;
  direction_token : Token.t;
  end_expr : expression;
  body : expression;
}

and apply_argument =
  | Positional of expression
  | Labeled of labeled_apply_argument
  | Optional of optional_apply_argument

and labeled_apply_argument = {
  syntax_node : syntax_node;
  label_token : Token.t;
  value : expression option;
}

and optional_apply_argument = {
  syntax_node : syntax_node;
  label_token : Token.t;
  value : expression option;
}

and apply_expression = {
  syntax_node : syntax_node;
  callee : expression;
  argument : apply_argument;
}

and method_call_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  method_name : Token.t;
}

and new_expression = {
  syntax_node : syntax_node;
  class_path : Ident.t;
}

and prefix_expression = {
  syntax_node : syntax_node;
  operator_token : Token.t;
  operand : expression;
}

and field_access_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  field_name : Token.t;
}

and index_expression = {
  syntax_node : syntax_node;
  collection : expression;
  index : expression;
}

and object_update_expression = {
  syntax_node : syntax_node;
  fields : record_expression_field list;
}

and instance_variable_assign_expression = {
  syntax_node : syntax_node;
  name_token : Token.t;
  operator_token : Token.t;
  value : expression;
}

and assign_expression = {
  syntax_node : syntax_node;
  target : expression;
  operator_token : Token.t;
  value : expression;
}

and infix_expression = {
  syntax_node : syntax_node;
  left : expression;
  operator_token : Token.t;
  right : expression;
}

and typed_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
}

and polymorphic_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
}

and coerce_expression = {
  syntax_node : syntax_node;
  expression : expression;
  from_type : core_type option;
  to_type : core_type;
}

and sequence_expression = {
  syntax_node : syntax_node;
  left : expression;
  right : expression;
}

and tuple_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

and list_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

and array_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

and record_expression =
  | Literal of record_literal_expression
  | Update of record_update_expression

and record_literal_expression = {
  syntax_node : syntax_node;
  fields : record_expression_field list;
}

and record_update_expression = {
  syntax_node : syntax_node;
  base : expression;
  fields : record_expression_field list;
}

and record_expression_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  value : expression option;
}

and local_open_expression = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  body : expression;
  via_let_open : bool;
}

and fun_expression = {
  syntax_node : syntax_node;
  parameters : Parameter.t list;
  body : expression;
}

and function_expression = {
  syntax_node : syntax_node;
  cases : match_case list;
}

and let_binding = {
  syntax_node : syntax_node;
  attributes : attribute list;
  binding_pattern : pattern;
  binding_name : Token.t option;
  parameters : Parameter.t list;
  value : expression;
  is_recursive : bool;
}

and binding_operator_binding = {
  keyword_token : Token.t;
  operator_token : Token.t;
  binding_pattern : pattern;
  bound_value : expression;
}

and let_operator_expression = {
  syntax_node : syntax_node;
  binding : binding_operator_binding;
  and_bindings : binding_operator_binding list;
  body : expression;
}

and let_expression = {
  syntax_node : syntax_node;
  binding_pattern : pattern;
  bound_value : expression;
  and_bindings : let_binding list;
  body : expression;
  is_recursive : bool;
}

and match_expression = {
  syntax_node : syntax_node;
  scrutinee : expression;
  cases : match_case list;
}

and try_expression = {
  syntax_node : syntax_node;
  body : expression;
  cases : match_case list;
}

and match_case = {
  syntax_node : syntax_node;
  pattern : pattern;
  guard : expression option;
  body : expression;
}

and if_expression = {
  syntax_node : syntax_node;
  condition : expression;
  then_branch : expression;
  else_branch : expression option;
}

and parenthesized_expression = {
  syntax_node : syntax_node;
  inner : expression;
}

and module_expression =
  | Path of Ident.t
  | Structure of {
      syntax_node : syntax_node;
      item_syntax_nodes : syntax_node list;
    }
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      body : module_expression;
    }
  | Apply of {
      syntax_node : syntax_node;
      callee : module_expression;
      argument : module_expression;
    }
  | Unpack of {
      syntax_node : syntax_node;
      expression : expression;
      module_type : module_type option;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_expression;
    }
  | Attribute of {
      syntax_node : syntax_node;
      module_expression : module_expression;
      attribute : attribute;
    }

module Expression = struct
  type t = expression =
    | Path of path_expression
    | Operator of operator_expression
    | Literal of literal
    | Unreachable of unreachable_expression
    | Attribute of attribute
    | Extension of extension
    | Object of object_expression
    | PolyVariant of poly_variant_expression
    | FirstClassModule of first_class_module_expression
    | LetModule of let_module_expression
    | LetException of let_exception_expression
    | Assert of assert_expression
    | Lazy of lazy_expression
    | While of while_expression
    | For of for_expression
    | Apply of apply_expression
    | MethodCall of method_call_expression
    | New of new_expression
    | Prefix of prefix_expression
    | FieldAccess of field_access_expression
    | Index of index_expression
    | ObjectUpdate of object_update_expression
    | InstanceVariableAssign of instance_variable_assign_expression
    | Assign of assign_expression
    | Infix of infix_expression
    | Typed of typed_expression
    | Polymorphic of polymorphic_expression
    | Coerce of coerce_expression
    | Sequence of sequence_expression
    | Tuple of tuple_expression
    | List of list_expression
    | Array of array_expression
    | Record of record_expression
    | LocalOpen of local_open_expression
    | Fun of fun_expression
    | Function of function_expression
    | LetOperator of let_operator_expression
    | Let of let_expression
    | Match of match_expression
    | Try of try_expression
    | If of if_expression
    | Parenthesized of parenthesized_expression

  let syntax_node = function
    | Path expr -> expr.syntax_node
    | Operator expr -> expr.syntax_node
    | Literal literal -> (
        match literal with
        | Literal.String { syntax_node; _ }
        | Literal.Int { syntax_node; _ }
        | Literal.Float { syntax_node; _ }
        | Literal.Char { syntax_node; _ }
        | Literal.Bool { syntax_node; _ }
        | Literal.Unit { syntax_node } ->
            syntax_node)
    | Unreachable expr -> expr.syntax_node
    | Attribute attr -> attr.syntax_node
    | Extension ext -> ext.syntax_node
    | Object expr -> expr.syntax_node
    | PolyVariant expr -> expr.syntax_node
    | FirstClassModule expr -> expr.syntax_node
    | LetModule expr -> expr.syntax_node
    | LetException expr -> expr.syntax_node
    | Assert expr -> expr.syntax_node
    | Lazy expr -> expr.syntax_node
    | While expr -> expr.syntax_node
    | For expr -> expr.syntax_node
    | Apply expr -> expr.syntax_node
    | MethodCall expr -> expr.syntax_node
    | New expr -> expr.syntax_node
    | Prefix expr -> expr.syntax_node
    | FieldAccess expr -> expr.syntax_node
    | Index expr -> expr.syntax_node
    | ObjectUpdate expr -> expr.syntax_node
    | InstanceVariableAssign expr -> expr.syntax_node
    | Assign expr -> expr.syntax_node
    | Infix expr -> expr.syntax_node
    | Typed expr -> expr.syntax_node
    | Polymorphic expr -> expr.syntax_node
    | Coerce expr -> expr.syntax_node
    | Sequence expr -> expr.syntax_node
    | Tuple expr -> expr.syntax_node
    | List expr -> expr.syntax_node
    | Array expr -> expr.syntax_node
    | Record expr -> (
        match expr with
        | Literal record -> record.syntax_node
        | Update record -> record.syntax_node)
    | LocalOpen expr -> expr.syntax_node
    | Fun expr -> expr.syntax_node
    | Function expr -> expr.syntax_node
    | LetOperator expr -> expr.syntax_node
    | Let expr -> expr.syntax_node
    | Match expr -> expr.syntax_node
    | Try expr -> expr.syntax_node
    | If expr -> expr.syntax_node
    | Parenthesized expr -> expr.syntax_node
end

module ModuleExpression = struct
  type t = module_expression =
    | Path of Ident.t
    | Structure of {
        syntax_node : syntax_node;
        item_syntax_nodes : syntax_node list;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        body : module_expression;
      }
    | Apply of {
        syntax_node : syntax_node;
        callee : module_expression;
        argument : module_expression;
      }
    | Unpack of {
        syntax_node : syntax_node;
        expression : expression;
        module_type : module_type option;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_expression;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_expression : module_expression;
        attribute : attribute;
      }

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Structure { syntax_node; _ }
    | Functor { syntax_node; _ }
    | Apply { syntax_node; _ }
    | Unpack { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
end

module Pattern = struct
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Attribute of attributed_pattern
    | Extension of extension
    | Literal of pattern_literal
    | Lazy of lazy_pattern
    | Exception of exception_pattern
    | Range of range_pattern
    | Operator of operator_pattern
    | FirstClassModule of first_class_module_pattern
    | PolyVariant of poly_variant_pattern
    | PolyVariantInherit of poly_variant_inherit_pattern
    | Constructor of constructor_pattern
    | Tuple of tuple_pattern
    | List of list_pattern
    | Array of array_pattern
    | Record of record_pattern
    | Cons of cons_pattern
    | Or of or_pattern
    | Alias of alias_pattern
    | Typed of typed_pattern
    | Effect of effect_pattern
    | LocalOpen of local_open_pattern
    | Parenthesized of parenthesized_pattern

  let syntax_node = function
    | Identifier pattern -> pattern.syntax_node
    | Wildcard pattern -> pattern.syntax_node
    | Attribute pattern -> pattern.syntax_node
    | Extension extension -> extension.syntax_node
    | Literal (PatternLiteral.String { syntax_node; _ })
    | Literal (PatternLiteral.Int { syntax_node; _ })
    | Literal (PatternLiteral.Float { syntax_node; _ })
    | Literal (PatternLiteral.Char { syntax_node; _ })
    | Literal (PatternLiteral.Bool { syntax_node; _ })
    | Literal (PatternLiteral.Unit { syntax_node }) ->
        syntax_node
    | Lazy pattern -> pattern.syntax_node
    | Exception pattern -> pattern.syntax_node
    | Range pattern -> pattern.syntax_node
    | Operator pattern -> pattern.syntax_node
    | FirstClassModule pattern -> pattern.syntax_node
    | PolyVariant pattern -> pattern.syntax_node
    | PolyVariantInherit pattern -> pattern.syntax_node
    | Constructor pattern -> pattern.syntax_node
    | Tuple pattern -> pattern.syntax_node
    | List pattern -> pattern.syntax_node
    | Array pattern -> pattern.syntax_node
    | Record pattern -> pattern.syntax_node
    | Cons pattern -> pattern.syntax_node
    | Or pattern -> pattern.syntax_node
    | Alias pattern -> pattern.syntax_node
    | Typed pattern -> pattern.syntax_node
    | Effect pattern -> pattern.syntax_node
    | LocalOpen pattern -> pattern.syntax_node
    | Parenthesized pattern -> pattern.syntax_node
end

module InfixExpression = struct
  type t = infix_expression = {
    syntax_node : syntax_node;
    left : expression;
    operator_token : Token.t;
    right : expression;
  }

  let syntax_node expr = expr.syntax_node
  let left expr = expr.left
  let operator_token expr = expr.operator_token
  let operator expr = Token.text expr.operator_token
  let right expr = expr.right
end

module RecordExpression = struct
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression

  let syntax_node = function
    | Literal expr -> expr.syntax_node
    | Update expr -> expr.syntax_node
end

module TypeVariable = struct
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  let syntax_node type_variable = type_variable.syntax_node
  let name_token type_variable = type_variable.name_token

  let text type_variable =
    Ceibo.Red.SyntaxNode.children type_variable.syntax_node
    |> Array.to_list
    |> List.filter_map (function
         | Ceibo.Red.Token tok
           when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
             Some (Ceibo.Red.SyntaxToken.text tok)
         | _ -> None)
    |> String.concat ""

  let name type_variable = Token.text type_variable.name_token
end

module TypeParameter = struct
  type t = {
    syntax_node : syntax_node;
    type_variable : TypeVariable.t option;
  }

  let syntax_node type_param = type_param.syntax_node
  let type_variable type_param = type_param.type_variable
end

module RecordField = struct
  type t = {
    syntax_node : syntax_node;
    field_name : Token.t;
    field_type : core_type;
    is_mutable : bool;
  }

  let syntax_node field = field.syntax_node
  let field_name_token field = field.field_name
  let field_type field = field.field_type
  let name field = Token.text field.field_name
  let is_mutable field = field.is_mutable
end

module VariantConstructor = struct
  type t = {
    syntax_node : syntax_node;
    constructor_name : Token.t;
    payload_type : core_type option;
  }

  let syntax_node constr = constr.syntax_node
  let constructor_name_token constr = constr.constructor_name
  let payload_type constr = constr.payload_type
  let name constr = Token.text constr.constructor_name
end

module PolyVariantTag = struct
  type t = poly_variant_tag = {
    syntax_node : syntax_node;
    tag_name : Token.t;
    payload_type : core_type option;
  }

  let syntax_node tag = tag.syntax_node
  let tag_name_token tag = tag.tag_name
  let payload_type tag = tag.payload_type
  let name tag = Token.text tag.tag_name
end

module PolyVariantBound = struct
  type t = poly_variant_bound =
    | Exact
    | UpperBound of {
        marker_token : Token.t;
      }
    | LowerBound of {
        marker_token : Token.t;
      }

  let marker_token = function
    | Exact -> None
    | UpperBound { marker_token } | LowerBound { marker_token } ->
        Some marker_token
end

module RowField = struct
  type t = row_field =
    | Tag of poly_variant_tag
    | Inherit of {
        syntax_node : syntax_node;
        type_ : core_type;
      }

  let syntax_node = function
    | Tag tag -> tag.syntax_node
    | Inherit { syntax_node; _ } -> syntax_node

  let tag = function
    | Tag tag -> Some tag
    | Inherit _ -> None

  let inherited_type = function
    | Tag _ -> None
    | Inherit { type_; _ } -> Some type_
end

module PolyVariant = struct
  type t = poly_variant = {
    syntax_node : syntax_node;
    kind : poly_variant_bound;
    fields : row_field list;
  }

  let syntax_node poly_variant = poly_variant.syntax_node
  let kind poly_variant = poly_variant.kind
  let fields poly_variant = poly_variant.fields

  let tags poly_variant =
    poly_variant.fields
    |> List.filter_map (function
         | RowField.Tag tag -> Some tag
         | RowField.Inherit _ -> None)
end

module TypeDefinition = struct
  type t =
    | Abstract
    | Alias of {
        syntax_node : syntax_node;
        manifest : core_type;
      }
    | Extensible of {
        syntax_node : syntax_node;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }
    | Record of RecordField.t list
    | Variant of VariantConstructor.t list
    | PolyVariant of PolyVariant.t
    | Other of syntax_node
end

module TypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    type_name : Ident.t;
    type_params : TypeParameter.t list;
    type_definition : TypeDefinition.t;
  }

  let syntax_node decl = decl.syntax_node
  let type_name decl = decl.type_name
  let type_params decl = decl.type_params
  let type_definition decl = decl.type_definition

  let name_token decl =
    match Ident.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeDeclaration.name_token: missing type name token"
end

module LetBinding = struct
  type t = let_binding = {
    syntax_node : syntax_node;
    attributes : attribute list;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : expression;
    is_recursive : bool;
  }

  let syntax_node binding = binding.syntax_node
  let attributes binding = binding.attributes
  let binding_pattern binding = binding.binding_pattern
  let binding_name_token binding = binding.binding_name
  let name binding =
    match binding.binding_name with
    | Some token -> Token.text token
    | None -> panic "LetBinding.name: missing binding name token"

  let parameters binding = binding.parameters
  let value binding = binding.value
  let value_syntax_node binding = Expression.syntax_node binding.value
  let is_recursive binding = binding.is_recursive

  let is_function binding =
    List.length binding.parameters > 0
    ||
    match Ceibo.Red.SyntaxNode.kind (value_syntax_node binding) with
    | Syntax_kind.FUN_EXPR | Syntax_kind.FUNCTION_EXPR -> true
    | _ -> false
end

module ModuleDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_name : Token.t;
    functor_parameters : functor_parameter list;
    module_type : module_type option;
    module_expression : module_expression option;
    is_recursive : bool;
  }

  let syntax_node decl = decl.syntax_node
  let module_name_token decl = decl.module_name
  let functor_parameters decl = decl.functor_parameters
  let module_type decl = decl.module_type
  let module_expression decl = decl.module_expression
  let is_recursive decl = decl.is_recursive
  let name decl = Token.text decl.module_name
end

module ModuleTypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
    module_type : module_type option;
  }

  let syntax_node decl = decl.syntax_node
  let module_type_name_token decl = decl.module_type_name
  let module_type decl = decl.module_type
  let name decl = Token.text decl.module_type_name
end

module OpenStatement = struct
  type t = {
    syntax_node : syntax_node;
    module_path : Ident.t;
    bang_token : Token.t option;
  }

  let syntax_node stmt = stmt.syntax_node
  let module_path stmt = stmt.module_path
  let bang_token stmt = stmt.bang_token
  let has_bang stmt = Option.is_some stmt.bang_token
end

type value_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
}

type external_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
  primitive_name_tokens : Token.t list;
}

type class_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_name : Token.t;
  class_type_syntax_node : syntax_node option;
  class_body : expression;
}

type class_type_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_type_name : Token.t;
  class_type_body_syntax_node : syntax_node;
}

type include_target =
  | ModuleExpression of module_expression
  | ModuleType of module_type

type include_statement = {
  syntax_node : syntax_node;
  target : include_target;
}

module Item = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | Expression of Expression.t
    | Attribute of attribute
    | Extension of extension
    | ClassDeclaration of class_declaration
    | ClassTypeDeclaration of class_type_declaration
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | ValueDeclaration of value_declaration
    | ExternalDeclaration of external_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | Expression expr -> Expression.syntax_node expr
    | Attribute attribute -> attribute.syntax_node
    | Extension extension -> extension.syntax_node
    | ClassDeclaration decl -> decl.syntax_node
    | ClassTypeDeclaration decl -> decl.syntax_node
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | ValueDeclaration decl -> decl.syntax_node
    | ExternalDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
end

type implementation = {
  syntax_node : syntax_node;
  items : Item.t list;
  let_bindings : LetBinding.t list;
  expressions : Expression.t list;
}

type interface = {
  syntax_node : syntax_node;
  items : Item.t list;
  let_bindings : LetBinding.t list;
  expressions : Expression.t list;
}

type t =
  | Implementation of implementation
  | Interface of interface

type source_file = t

module SourceFile = struct
  type t = source_file

  let syntax_node = function
    | Implementation source_file -> source_file.syntax_node
    | Interface source_file -> source_file.syntax_node

  let items = function
    | Implementation source_file -> source_file.items
    | Interface source_file -> source_file.items

  let let_bindings = function
    | Implementation source_file -> source_file.let_bindings
    | Interface source_file -> source_file.let_bindings

  let expressions = function
    | Implementation source_file -> source_file.expressions
    | Interface source_file -> source_file.expressions

  let kind = function
    | Implementation _ -> `Implementation
    | Interface _ -> `Interface
end

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
