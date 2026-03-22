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
  | Parenthesized of parenthesized_pattern
  | Unknown of syntax_node

and identifier_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

and wildcard_pattern = {
  syntax_node : syntax_node;
}

and parenthesized_pattern = {
  syntax_node : syntax_node;
  inner : pattern;
}

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
  | Apply of apply_expression
  | Infix of infix_expression
  | Let of let_expression
  | Match of match_expression
  | If of if_expression
  | Parenthesized of parenthesized_expression
  | Unknown of syntax_node

and path_expression = {
  syntax_node : syntax_node;
  path : ModulePath.t;
}

and apply_expression = {
  syntax_node : syntax_node;
  callee : expression;
  argument : expression;
}

and infix_expression = {
  syntax_node : syntax_node;
  left : expression;
  operator_token : Token.t;
  right : expression;
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
    | Apply of apply_expression
    | Infix of infix_expression
    | Let of let_expression
    | Match of match_expression
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
    | Parenthesized of parenthesized_pattern
    | Unknown of syntax_node

  val syntax_node : t -> syntax_node
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

module ApplyExpression : sig
  type t = apply_expression = {
    syntax_node : syntax_node;
    callee : expression;
    argument : expression;
  }

  val syntax_node : t -> syntax_node
  val callee : t -> Expression.t
  val argument : t -> Expression.t
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
  val cases : t -> MatchCase.t list
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

module LetBinding : sig
  type t = {
    syntax_node : syntax_node;
    binding_name : Token.t;
    parameters : Parameter.t list;
    value : Expression.t;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
  val binding_name_token : t -> Token.t
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

module Item : sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
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

val of_green_tree : green_node -> source_file
val syntax_node_of_source_file : source_file -> syntax_node
