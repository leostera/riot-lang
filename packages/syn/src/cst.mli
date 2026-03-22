open Std

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type green_node = (Syntax_kind.t, string) Ceibo.Green.node

module Token : sig
  type t = { syntax_token : syntax_token }

  val syntax_token : t -> syntax_token
  val text : t -> string
  val span : t -> Ceibo.Span.t
end

type attribute = {
  syntax_node : syntax_node;
  tokens : Token.t list;
}

type extension = {
  syntax_node : syntax_node;
  tokens : Token.t list;
}

module ModulePath : sig
  type t = {
    syntax_node : syntax_node;
    segments : Token.t list;
  }

  val syntax_node : t -> syntax_node
  val segments : t -> Token.t list
  val last_segment : t -> Token.t option
  val name : t -> string option
end

type object_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
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
      constructor_path : ModulePath.t;
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
  | PolyVariant of {
      syntax_node : syntax_node;
      tags : poly_variant_tag list;
    }
  | Record of {
      syntax_node : syntax_node;
      fields : record_type_field list;
    }
  | FirstClassModule of {
      syntax_node : syntax_node;
      module_type_syntax_node : syntax_node;
    }
  | Object of {
      syntax_node : syntax_node;
      fields : object_type_field list;
    }

module CoreType : sig
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
        constructor_path : ModulePath.t;
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
    | PolyVariant of {
        syntax_node : syntax_node;
        tags : poly_variant_tag list;
      }
    | Record of {
        syntax_node : syntax_node;
        fields : record_type_field list;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type_syntax_node : syntax_node;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }

  val syntax_node : t -> syntax_node
end

module PatternLiteral : sig
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
  module_type_syntax_node : syntax_node option;
}

and poly_variant_pattern = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : pattern option;
}

and poly_variant_inherit_pattern = {
  syntax_node : syntax_node;
  type_path : ModulePath.t;
}

and constructor_pattern = {
  syntax_node : syntax_node;
  constructor_path : ModulePath.t;
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
  field_path : ModulePath.t;
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

type parameter =
  | Positional of positional_parameter
  | Labeled of labeled_parameter
  | Optional of optional_parameter
  | LocallyAbstract of syntax_node

module Parameter : sig
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of syntax_node

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t option
  val name : t -> string option
  val is_named : t -> bool
  val has_default : t -> bool
end

module Literal : sig
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
  | Assign of assign_expression
  | Infix of infix_expression
  | Typed of typed_expression
  | Coerce of coerce_expression
  | Sequence of sequence_expression
  | Tuple of tuple_expression
  | List of list_expression
  | Array of array_expression
  | Record of record_expression
  | LocalOpen of local_open_expression
  | Fun of fun_expression
  | Function of function_expression
  | Let of let_expression
  | Match of match_expression
  | Try of try_expression
  | If of if_expression
  | Parenthesized of parenthesized_expression

and path_expression = {
  syntax_node : syntax_node;
  path : ModulePath.t;
}

and operator_expression = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
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
  module_syntax_node : syntax_node;
  module_type_syntax_node : syntax_node option;
}

and let_module_expression = {
  syntax_node : syntax_node;
  module_name_token : Token.t;
  module_expression_syntax_node : syntax_node;
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
  class_path : ModulePath.t;
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
  field_path : ModulePath.t;
  value : expression option;
}

and local_open_expression = {
  syntax_node : syntax_node;
  module_path : ModulePath.t;
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

module Expression : sig
  type t = expression =
    | Path of path_expression
    | Operator of operator_expression
    | Literal of literal
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
    | Assign of assign_expression
    | Infix of infix_expression
    | Typed of typed_expression
    | Coerce of coerce_expression
    | Sequence of sequence_expression
    | Tuple of tuple_expression
    | List of list_expression
    | Array of array_expression
    | Record of record_expression
    | LocalOpen of local_open_expression
    | Fun of fun_expression
    | Function of function_expression
    | Let of let_expression
    | Match of match_expression
    | Try of try_expression
    | If of if_expression
    | Parenthesized of parenthesized_expression

  val syntax_node : t -> syntax_node
end

module Pattern : sig
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
    | Parenthesized of parenthesized_pattern

  val syntax_node : t -> syntax_node
end

module InfixExpression : sig
  type t = infix_expression = {
    syntax_node : syntax_node;
    left : expression;
    operator_token : Token.t;
    right : expression;
  }

  val syntax_node : t -> syntax_node
  val left : t -> Expression.t
  val operator_token : t -> Token.t
  val operator : t -> string
  val right : t -> Expression.t
end

module RecordExpression : sig
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression

  val syntax_node : t -> syntax_node
end

module TypeVariable : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t
  val name : t -> string
  val text : t -> string
end

module TypeParameter : sig
  type t = {
    syntax_node : syntax_node;
    type_variable : TypeVariable.t option;
  }

  val syntax_node : t -> syntax_node
  val type_variable : t -> TypeVariable.t option
end

module RecordField : sig
  type t = {
    syntax_node : syntax_node;
    field_name : Token.t;
    field_type : core_type;
    is_mutable : bool;
  }

  val syntax_node : t -> syntax_node
  val field_name_token : t -> Token.t
  val field_type : t -> core_type
  val name : t -> string
  val is_mutable : t -> bool
end

module VariantConstructor : sig
  type t = {
    syntax_node : syntax_node;
    constructor_name : Token.t;
    payload_type : core_type option;
  }

  val syntax_node : t -> syntax_node
  val constructor_name_token : t -> Token.t
  val payload_type : t -> core_type option
  val name : t -> string
end

module PolyVariantTag : sig
  type t = poly_variant_tag = {
    syntax_node : syntax_node;
    tag_name : Token.t;
    payload_type : core_type option;
  }

  val syntax_node : t -> syntax_node
  val tag_name_token : t -> Token.t
  val payload_type : t -> core_type option
  val name : t -> string
end

module TypeDefinition : sig
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
        module_type_syntax_node : syntax_node;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }
    | Record of RecordField.t list
    | Variant of VariantConstructor.t list
    | PolyVariant of PolyVariantTag.t list
    | Other of syntax_node
end

module TypeDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    type_name : ModulePath.t;
    type_params : TypeParameter.t list;
    type_definition : TypeDefinition.t;
  }

  val syntax_node : t -> syntax_node
  val type_name : t -> ModulePath.t
  val type_params : t -> TypeParameter.t list
  val type_definition : t -> TypeDefinition.t
  val name_token : t -> Token.t
end

module LetBinding : sig
  type t = let_binding = {
    syntax_node : syntax_node;
    attributes : attribute list;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : expression;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
  val attributes : t -> attribute list
  val binding_pattern : t -> Pattern.t
  val binding_name_token : t -> Token.t option
  val name : t -> string
  val parameters : t -> Parameter.t list
  val value : t -> Expression.t
  val value_syntax_node : t -> syntax_node
  val is_recursive : t -> bool
  val is_function : t -> bool
end

module ModuleDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    module_name : Token.t;
  }

  val syntax_node : t -> syntax_node
  val module_name_token : t -> Token.t
  val name : t -> string
end

module ModuleTypeDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
  }

  val syntax_node : t -> syntax_node
  val module_type_name_token : t -> Token.t
  val name : t -> string
end

module OpenStatement : sig
  type t = {
    syntax_node : syntax_node;
    module_path : ModulePath.t;
    bang_token : Token.t option;
  }

  val syntax_node : t -> syntax_node
  val module_path : t -> ModulePath.t
  val bang_token : t -> Token.t option
  val has_bang : t -> bool
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

type include_statement = {
  syntax_node : syntax_node;
  included_syntax_node : syntax_node;
}

module Item : sig
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

  val syntax_node : t -> syntax_node
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

module SourceFile : sig
  type t = source_file

  val syntax_node : t -> syntax_node
  val items : t -> Item.t list
  val let_bindings : t -> LetBinding.t list
  val expressions : t -> Expression.t list
  val kind : t -> [ `Implementation | `Interface ]
end

val syntax_node_of_source_file : source_file -> syntax_node
