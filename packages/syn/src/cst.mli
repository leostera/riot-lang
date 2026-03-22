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
  | Literal of pattern_literal
  | PolyVariant of poly_variant_pattern
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
  | Unknown of syntax_node

and identifier_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

and wildcard_pattern = {
  syntax_node : syntax_node;
}

and poly_variant_pattern = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : pattern option;
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
  type_syntax_node : syntax_node;
}

and parenthesized_pattern = {
  syntax_node : syntax_node;
  inner : pattern;
}

module PositionalParameter : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t option;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t option
  val name : t -> string option
end

module LabeledParameter : sig
  type t = {
    syntax_node : syntax_node;
    label_token : Token.t;
    binding_name_token : Token.t option;
  }

  val syntax_node : t -> syntax_node
  val label_token : t -> Token.t
  val label : t -> string
  val binding_name_token : t -> Token.t option
end

module OptionalParameter : sig
  type t = {
    syntax_node : syntax_node;
    label_token : Token.t;
    binding_name_token : Token.t option;
    has_default : bool;
  }

  val syntax_node : t -> syntax_node
  val label_token : t -> Token.t
  val label : t -> string
  val binding_name_token : t -> Token.t option
  val has_default : t -> bool
end

module Parameter : sig
  type t =
    | Positional of PositionalParameter.t
    | Labeled of LabeledParameter.t
    | Optional of OptionalParameter.t
    | LocallyAbstract of syntax_node
    | Unknown of syntax_node

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

type expression =
  | Path of path_expression
  | Literal of literal
  | PolyVariant of poly_variant_expression
  | Assert of assert_expression
  | Lazy of lazy_expression
  | While of while_expression
  | For of for_expression
  | Apply of apply_expression
  | Prefix of prefix_expression
  | FieldAccess of field_access_expression
  | Index of index_expression
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
  | Unknown of syntax_node

and path_expression = {
  syntax_node : syntax_node;
  path : ModulePath.t;
}

and poly_variant_expression = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : expression option;
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
  type_syntax_node : syntax_node;
}

and coerce_expression = {
  syntax_node : syntax_node;
  expression : expression;
  from_type_syntax_node : syntax_node option;
  to_type_syntax_node : syntax_node;
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

and let_expression = {
  syntax_node : syntax_node;
  binding_pattern : pattern;
  bound_value : expression;
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
    | Literal of literal
    | PolyVariant of poly_variant_expression
    | Assert of assert_expression
    | Lazy of lazy_expression
    | While of while_expression
    | For of for_expression
    | Apply of apply_expression
    | Prefix of prefix_expression
    | FieldAccess of field_access_expression
    | Index of index_expression
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
    | Unknown of syntax_node

  val syntax_node : t -> syntax_node
end

module Pattern : sig
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Literal of pattern_literal
    | PolyVariant of poly_variant_pattern
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
    | Unknown of syntax_node

  val syntax_node : t -> syntax_node
end

module PolyVariantPattern : sig
  type t = poly_variant_pattern = {
    syntax_node : syntax_node;
    tag_token : Token.t;
    payload : pattern option;
  }

  val syntax_node : t -> syntax_node
  val tag_token : t -> Token.t
  val tag : t -> string
  val payload : t -> Pattern.t option
end

module ArrayPattern : sig
  type t = array_pattern = {
    syntax_node : syntax_node;
    elements : pattern list;
  }

  val syntax_node : t -> syntax_node
  val elements : t -> Pattern.t list
end

module RecordPattern : sig
  type t = record_pattern = {
    syntax_node : syntax_node;
    fields : record_pattern_field list;
  }

  val syntax_node : t -> syntax_node
  val fields : t -> record_pattern_field list
end

module RecordPatternField : sig
  type t = record_pattern_field = {
    syntax_node : syntax_node;
    field_path : ModulePath.t;
    pattern : pattern option;
  }

  val syntax_node : t -> syntax_node
  val field_path : t -> ModulePath.t
  val pattern : t -> Pattern.t option
end

module ParenthesizedPattern : sig
  type t = parenthesized_pattern = {
    syntax_node : syntax_node;
    inner : pattern;
  }

  val syntax_node : t -> syntax_node
  val inner : t -> Pattern.t
end

module PathExpression : sig
  type t = path_expression = {
    syntax_node : syntax_node;
    path : ModulePath.t;
  }

  val syntax_node : t -> syntax_node
  val path : t -> ModulePath.t
end

module PolyVariantExpression : sig
  type t = poly_variant_expression = {
    syntax_node : syntax_node;
    tag_token : Token.t;
    payload : expression option;
  }

  val syntax_node : t -> syntax_node
  val tag_token : t -> Token.t
  val tag : t -> string
  val payload : t -> Expression.t option
end

module AssertExpression : sig
  type t = assert_expression = {
    syntax_node : syntax_node;
    asserted : expression;
  }

  val syntax_node : t -> syntax_node
  val asserted : t -> Expression.t
end

module LazyExpression : sig
  type t = lazy_expression = {
    syntax_node : syntax_node;
    body : expression;
  }

  val syntax_node : t -> syntax_node
  val body : t -> Expression.t
end

module WhileExpression : sig
  type t = while_expression = {
    syntax_node : syntax_node;
    condition : expression;
    body : expression;
  }

  val syntax_node : t -> syntax_node
  val condition : t -> Expression.t
  val body : t -> Expression.t
end

module ForExpression : sig
  type t = for_expression = {
    syntax_node : syntax_node;
    iterator_token : Token.t;
    start_expr : expression;
    direction_token : Token.t;
    end_expr : expression;
    body : expression;
  }

  val syntax_node : t -> syntax_node
  val iterator_token : t -> Token.t
  val iterator_name : t -> string
  val start_expr : t -> Expression.t
  val direction_token : t -> Token.t
  val direction : t -> string
  val end_expr : t -> Expression.t
  val body : t -> Expression.t
end

module ApplyExpression : sig
  type t = apply_expression = {
    syntax_node : syntax_node;
    callee : expression;
    argument : apply_argument;
  }

  val syntax_node : t -> syntax_node
  val callee : t -> Expression.t
  val argument : t -> apply_argument
end

module IndexExpression : sig
  type t = index_expression = {
    syntax_node : syntax_node;
    collection : expression;
    index : expression;
  }

  val syntax_node : t -> syntax_node
  val collection : t -> Expression.t
  val index : t -> Expression.t
end

module AssignExpression : sig
  type t = assign_expression = {
    syntax_node : syntax_node;
    target : expression;
    operator_token : Token.t;
    value : expression;
  }

  val syntax_node : t -> syntax_node
  val target : t -> Expression.t
  val operator_token : t -> Token.t
  val operator : t -> string
  val value : t -> Expression.t
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

module RecordExpressionField : sig
  type t = record_expression_field = {
    syntax_node : syntax_node;
    field_path : ModulePath.t;
    value : expression option;
  }

  val syntax_node : t -> syntax_node
  val field_path : t -> ModulePath.t
  val value : t -> Expression.t option
end

module FunExpression : sig
  type t = fun_expression = {
    syntax_node : syntax_node;
    parameters : Parameter.t list;
    body : expression;
  }

  val syntax_node : t -> syntax_node
  val parameters : t -> Parameter.t list
  val body : t -> Expression.t
end

module FunctionExpression : sig
  type t = function_expression = {
    syntax_node : syntax_node;
    cases : match_case list;
  }

  val syntax_node : t -> syntax_node
  val cases : t -> match_case list
end

module LetExpression : sig
  type t = let_expression = {
    syntax_node : syntax_node;
    binding_pattern : pattern;
    bound_value : expression;
    body : expression;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
  val binding_pattern : t -> Pattern.t
  val bound_value : t -> Expression.t
  val body : t -> Expression.t
  val is_recursive : t -> bool
end

module MatchCase : sig
  type t = match_case = {
    syntax_node : syntax_node;
    pattern : pattern;
    guard : expression option;
    body : expression;
  }

  val syntax_node : t -> syntax_node
  val pattern : t -> Pattern.t
  val guard : t -> Expression.t option
  val body : t -> Expression.t
end

module MatchExpression : sig
  type t = match_expression = {
    syntax_node : syntax_node;
    scrutinee : expression;
    cases : match_case list;
  }

  val syntax_node : t -> syntax_node
  val scrutinee : t -> Expression.t
  val cases : t -> match_case list
end

module TryExpression : sig
  type t = try_expression = {
    syntax_node : syntax_node;
    body : expression;
    cases : match_case list;
  }

  val syntax_node : t -> syntax_node
  val body : t -> Expression.t
  val cases : t -> match_case list
end

module IfExpression : sig
  type t = if_expression = {
    syntax_node : syntax_node;
    condition : expression;
    then_branch : expression;
    else_branch : expression option;
  }

  val syntax_node : t -> syntax_node
  val condition : t -> Expression.t
  val then_branch : t -> Expression.t
  val else_branch : t -> Expression.t option
end

module ParenthesizedExpression : sig
  type t = parenthesized_expression = {
    syntax_node : syntax_node;
    inner : expression;
  }

  val syntax_node : t -> syntax_node
  val inner : t -> Expression.t
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
    is_mutable : bool;
  }

  val syntax_node : t -> syntax_node
  val field_name_token : t -> Token.t
  val name : t -> string
  val is_mutable : t -> bool
end

module VariantConstructor : sig
  type t = {
    syntax_node : syntax_node;
    constructor_name : Token.t;
  }

  val syntax_node : t -> syntax_node
  val constructor_name_token : t -> Token.t
  val name : t -> string
end

module PolyVariantTag : sig
  type t = {
    syntax_node : syntax_node;
    tag_name : Token.t;
  }

  val syntax_node : t -> syntax_node
  val tag_name_token : t -> Token.t
  val name : t -> string
end

module TypeDefinition : sig
  type t =
    | Abstract
    | Alias of {
        syntax_node : syntax_node;
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
  type t = {
    syntax_node : syntax_node;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : Expression.t;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
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

module ValueDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
    type_syntax_node : syntax_node;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t
  val name : t -> string
  val type_syntax_node : t -> syntax_node
end

module ExternalDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
    type_syntax_node : syntax_node;
    primitive_name_tokens : Token.t list;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t
  val name : t -> string
  val type_syntax_node : t -> syntax_node
  val primitive_name_tokens : t -> Token.t list
end

module IncludeStatement : sig
  type t = {
    syntax_node : syntax_node;
    included_syntax_node : syntax_node;
  }

  val syntax_node : t -> syntax_node
  val included_syntax_node : t -> syntax_node
end

module Item : sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | Expression of Expression.t
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | ValueDeclaration of ValueDeclaration.t
    | ExternalDeclaration of ExternalDeclaration.t
    | IncludeStatement of IncludeStatement.t
    | Unknown of syntax_node

  val syntax_node : t -> syntax_node
end

module SourceFile : sig
  type t = {
    syntax_node : syntax_node;
    items : Item.t list;
    let_bindings : LetBinding.t list;
    expressions : Expression.t list;
  }

  val syntax_node : t -> syntax_node
  val items : t -> Item.t list
  val let_bindings : t -> LetBinding.t list
  val expressions : t -> Expression.t list
end

type source_file = SourceFile.t

val syntax_node_of_source_file : source_file -> syntax_node
