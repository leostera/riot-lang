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

module ModulePath = struct
  type t = {
    syntax_node : syntax_node;
    segments : Token.t list;
  }

  let syntax_node path = path.syntax_node
  let segments path = path.segments
  let last_segment path =
    match List.rev path.segments with
    | segment :: _ -> Some segment
    | [] -> None

  let name path =
    match last_segment path with
    | Some segment -> Some (Token.text segment)
    | None -> None
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

type pattern_literal = PatternLiteral.t

type pattern =
  | Identifier of identifier_pattern
  | Wildcard of wildcard_pattern
  | Literal of pattern_literal
  | Lazy of lazy_pattern
  | Exception of exception_pattern
  | Range of range_pattern
  | FirstClassModule of first_class_module_pattern
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

module PositionalParameter = struct
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t option;
  }

  let syntax_node param = param.syntax_node
  let name_token param = param.name_token

  let name param =
    match param.name_token with
    | Some token -> Some (Token.text token)
    | None -> None
end

module LabeledParameter = struct
  type t = {
    syntax_node : syntax_node;
    label_token : Token.t;
    binding_name_token : Token.t option;
  }

  let syntax_node param = param.syntax_node
  let label_token param = param.label_token
  let label param = Token.text param.label_token
  let binding_name_token param = param.binding_name_token
end

module OptionalParameter = struct
  type t = {
    syntax_node : syntax_node;
    label_token : Token.t;
    binding_name_token : Token.t option;
    has_default : bool;
  }

  let syntax_node param = param.syntax_node
  let label_token param = param.label_token
  let label param = Token.text param.label_token
  let binding_name_token param = param.binding_name_token
  let has_default param = param.has_default
end

module Parameter = struct
  type t =
    | Positional of PositionalParameter.t
    | Labeled of LabeledParameter.t
    | Optional of OptionalParameter.t
    | LocallyAbstract of syntax_node
    | Unknown of syntax_node

  let syntax_node = function
    | Positional param -> PositionalParameter.syntax_node param
    | Labeled param -> LabeledParameter.syntax_node param
    | Optional param -> OptionalParameter.syntax_node param
    | LocallyAbstract node -> node
    | Unknown node -> node

  let name_token = function
    | Positional param -> PositionalParameter.name_token param
    | Labeled param -> Some (LabeledParameter.label_token param)
    | Optional param -> Some (OptionalParameter.label_token param)
    | LocallyAbstract _ | Unknown _ -> None

  let name param =
    match name_token param with
    | Some token -> Some (Token.text token)
    | None -> None

  let is_named = function
    | Labeled _ | Optional _ -> true
    | Positional _ | LocallyAbstract _ | Unknown _ -> false

  let has_default = function
    | Optional param -> OptionalParameter.has_default param
    | Positional _ | Labeled _ | LocallyAbstract _ | Unknown _ -> false
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

type expression =
  | Path of path_expression
  | Literal of literal
  | PolyVariant of poly_variant_expression
  | FirstClassModule of first_class_module_expression
  | LetModule of let_module_expression
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

module Expression = struct
  type t = expression =
    | Path of path_expression
    | Literal of literal
    | PolyVariant of poly_variant_expression
    | FirstClassModule of first_class_module_expression
    | LetModule of let_module_expression
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

  let syntax_node = function
    | Path expr -> expr.syntax_node
    | Literal literal -> (
        match literal with
        | Literal.String { syntax_node; _ }
        | Literal.Int { syntax_node; _ }
        | Literal.Float { syntax_node; _ }
        | Literal.Char { syntax_node; _ }
        | Literal.Bool { syntax_node; _ }
        | Literal.Unit { syntax_node } ->
            syntax_node)
    | PolyVariant expr -> expr.syntax_node
    | FirstClassModule expr -> expr.syntax_node
    | LetModule expr -> expr.syntax_node
    | Assert expr -> expr.syntax_node
    | Lazy expr -> expr.syntax_node
    | While expr -> expr.syntax_node
    | For expr -> expr.syntax_node
    | Apply expr -> expr.syntax_node
    | Prefix expr -> expr.syntax_node
    | FieldAccess expr -> expr.syntax_node
    | Index expr -> expr.syntax_node
    | Assign expr -> expr.syntax_node
    | Infix expr -> expr.syntax_node
    | Typed expr -> expr.syntax_node
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
    | Let expr -> expr.syntax_node
    | Match expr -> expr.syntax_node
    | Try expr -> expr.syntax_node
    | If expr -> expr.syntax_node
    | Parenthesized expr -> expr.syntax_node
    | Unknown node -> node
end

module Pattern = struct
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Literal of pattern_literal
    | Lazy of lazy_pattern
    | Exception of exception_pattern
    | Range of range_pattern
    | FirstClassModule of first_class_module_pattern
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

  let syntax_node = function
    | Identifier pattern -> pattern.syntax_node
    | Wildcard pattern -> pattern.syntax_node
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
    | FirstClassModule pattern -> pattern.syntax_node
    | PolyVariant pattern -> pattern.syntax_node
    | Constructor pattern -> pattern.syntax_node
    | Tuple pattern -> pattern.syntax_node
    | List pattern -> pattern.syntax_node
    | Array pattern -> pattern.syntax_node
    | Record pattern -> pattern.syntax_node
    | Cons pattern -> pattern.syntax_node
    | Or pattern -> pattern.syntax_node
    | Alias pattern -> pattern.syntax_node
    | Typed pattern -> pattern.syntax_node
    | Parenthesized pattern -> pattern.syntax_node
    | Unknown node -> node
end

module IdentifierPattern = struct
  type t = identifier_pattern = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  let syntax_node pattern = pattern.syntax_node
  let name_token pattern = pattern.name_token
  let name pattern = Token.text pattern.name_token
end

module WildcardPattern = struct
  type t = wildcard_pattern = {
    syntax_node : syntax_node;
  }

  let syntax_node pattern = pattern.syntax_node
end

module PolyVariantPattern = struct
  type t = poly_variant_pattern = {
    syntax_node : syntax_node;
    tag_token : Token.t;
    payload : pattern option;
  }

  let syntax_node pattern = pattern.syntax_node
  let tag_token pattern = pattern.tag_token
  let tag pattern = Token.text pattern.tag_token
  let payload pattern = pattern.payload
end

module ParenthesizedPattern = struct
  type t = parenthesized_pattern = {
    syntax_node : syntax_node;
    inner : pattern;
  }

  let syntax_node pattern = pattern.syntax_node
  let inner pattern = pattern.inner
end

module ArrayPattern = struct
  type t = array_pattern = {
    syntax_node : syntax_node;
    elements : pattern list;
  }

  let syntax_node pattern = pattern.syntax_node
  let elements pattern = pattern.elements
end

module RecordPattern = struct
  type t = record_pattern = {
    syntax_node : syntax_node;
    fields : record_pattern_field list;
  }

  let syntax_node pattern = pattern.syntax_node
  let fields pattern = pattern.fields
end

module RecordPatternField = struct
  type t = record_pattern_field = {
    syntax_node : syntax_node;
    field_path : ModulePath.t;
    pattern : pattern option;
  }

  let syntax_node field = field.syntax_node
  let field_path field = field.field_path
  let pattern field = field.pattern
end

module PathExpression = struct
  type t = path_expression = {
    syntax_node : syntax_node;
    path : ModulePath.t;
  }

  let syntax_node expr = expr.syntax_node
  let path expr = expr.path
end

module PolyVariantExpression = struct
  type t = poly_variant_expression = {
    syntax_node : syntax_node;
    tag_token : Token.t;
    payload : expression option;
  }

  let syntax_node expr = expr.syntax_node
  let tag_token expr = expr.tag_token
  let tag expr = Token.text expr.tag_token
  let payload expr = expr.payload
end

module AssertExpression = struct
  type t = assert_expression = {
    syntax_node : syntax_node;
    asserted : expression;
  }

  let syntax_node expr = expr.syntax_node
  let asserted expr = expr.asserted
end

module LazyExpression = struct
  type t = lazy_expression = {
    syntax_node : syntax_node;
    body : expression;
  }

  let syntax_node expr = expr.syntax_node
  let body expr = expr.body
end

module WhileExpression = struct
  type t = while_expression = {
    syntax_node : syntax_node;
    condition : expression;
    body : expression;
  }

  let syntax_node expr = expr.syntax_node
  let condition expr = expr.condition
  let body expr = expr.body
end

module ForExpression = struct
  type t = for_expression = {
    syntax_node : syntax_node;
    iterator_token : Token.t;
    start_expr : expression;
    direction_token : Token.t;
    end_expr : expression;
    body : expression;
  }

  let syntax_node expr = expr.syntax_node
  let iterator_token expr = expr.iterator_token
  let iterator_name expr = Token.text expr.iterator_token
  let start_expr expr = expr.start_expr
  let direction_token expr = expr.direction_token
  let direction expr = Token.text expr.direction_token
  let end_expr expr = expr.end_expr
  let body expr = expr.body
end


module ApplyExpression = struct
  type t = apply_expression = {
    syntax_node : syntax_node;
    callee : expression;
    argument : apply_argument;
  }

  let syntax_node expr = expr.syntax_node
  let callee expr = expr.callee
  let argument expr = expr.argument
end

module IndexExpression = struct
  type t = index_expression = {
    syntax_node : syntax_node;
    collection : expression;
    index : expression;
  }

  let syntax_node expr = expr.syntax_node
  let collection expr = expr.collection
  let index expr = expr.index
end

module AssignExpression = struct
  type t = assign_expression = {
    syntax_node : syntax_node;
    target : expression;
    operator_token : Token.t;
    value : expression;
  }

  let syntax_node expr = expr.syntax_node
  let target expr = expr.target
  let operator_token expr = expr.operator_token
  let operator expr = Token.text expr.operator_token
  let value expr = expr.value
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

module RecordExpressionField = struct
  type t = record_expression_field = {
    syntax_node : syntax_node;
    field_path : ModulePath.t;
    value : expression option;
  }

  let syntax_node field = field.syntax_node
  let field_path field = field.field_path
  let value field = field.value
end

module FunExpression = struct
  type t = fun_expression = {
    syntax_node : syntax_node;
    parameters : Parameter.t list;
    body : expression;
  }

  let syntax_node expr = expr.syntax_node
  let parameters expr = expr.parameters
  let body expr = expr.body
end

module FunctionExpression = struct
  type t = function_expression = {
    syntax_node : syntax_node;
    cases : match_case list;
  }

  let syntax_node expr = expr.syntax_node
  let cases expr = expr.cases
end

module LetExpression = struct
  type t = let_expression = {
    syntax_node : syntax_node;
    binding_pattern : pattern;
    bound_value : expression;
    body : expression;
    is_recursive : bool;
  }

  let syntax_node expr = expr.syntax_node
  let binding_pattern expr = expr.binding_pattern
  let bound_value expr = expr.bound_value
  let body expr = expr.body
  let is_recursive expr = expr.is_recursive
end

module MatchCase = struct
  type t = match_case = {
    syntax_node : syntax_node;
    pattern : pattern;
    guard : expression option;
    body : expression;
  }

  let syntax_node case = case.syntax_node
  let pattern case = case.pattern
  let guard case = case.guard
  let body case = case.body
end

module MatchExpression = struct
  type t = match_expression = {
    syntax_node : syntax_node;
    scrutinee : expression;
    cases : match_case list;
  }

  let syntax_node expr = expr.syntax_node
  let scrutinee expr = expr.scrutinee
  let cases expr = expr.cases
end

module TryExpression = struct
  type t = try_expression = {
    syntax_node : syntax_node;
    body : expression;
    cases : match_case list;
  }

  let syntax_node expr = expr.syntax_node
  let body expr = expr.body
  let cases expr = expr.cases
end

module IfExpression = struct
  type t = if_expression = {
    syntax_node : syntax_node;
    condition : expression;
    then_branch : expression;
    else_branch : expression option;
  }

  let syntax_node expr = expr.syntax_node
  let condition expr = expr.condition
  let then_branch expr = expr.then_branch
  let else_branch expr = expr.else_branch
end

module ParenthesizedExpression = struct
  type t = parenthesized_expression = {
    syntax_node : syntax_node;
    inner : expression;
  }

  let syntax_node expr = expr.syntax_node
  let inner expr = expr.inner
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
    is_mutable : bool;
  }

  let syntax_node field = field.syntax_node
  let field_name_token field = field.field_name
  let name field = Token.text field.field_name
  let is_mutable field = field.is_mutable
end

module VariantConstructor = struct
  type t = {
    syntax_node : syntax_node;
    constructor_name : Token.t;
  }

  let syntax_node constr = constr.syntax_node
  let constructor_name_token constr = constr.constructor_name
  let name constr = Token.text constr.constructor_name
end

module PolyVariantTag = struct
  type t = {
    syntax_node : syntax_node;
    tag_name : Token.t;
  }

  let syntax_node tag = tag.syntax_node
  let tag_name_token tag = tag.tag_name
  let name tag = Token.text tag.tag_name
end

module TypeDefinition = struct
  type t =
    | Abstract
    | Alias of {
        syntax_node : syntax_node;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type_syntax_node : syntax_node;
      }
    | Record of RecordField.t list
    | Variant of VariantConstructor.t list
    | PolyVariant of PolyVariantTag.t list
    | Other of syntax_node
end

module TypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    type_name : ModulePath.t;
    type_params : TypeParameter.t list;
    type_definition : TypeDefinition.t;
  }

  let syntax_node decl = decl.syntax_node
  let type_name decl = decl.type_name
  let type_params decl = decl.type_params
  let type_definition decl = decl.type_definition

  let name_token decl =
    match ModulePath.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeDeclaration.name_token: missing type name token"
end

module LetBinding = struct
  type t = {
    syntax_node : syntax_node;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : Expression.t;
    is_recursive : bool;
  }

  let syntax_node binding = binding.syntax_node
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
  }

  let syntax_node decl = decl.syntax_node
  let module_name_token decl = decl.module_name
  let name decl = Token.text decl.module_name
end

module ModuleTypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
  }

  let syntax_node decl = decl.syntax_node
  let module_type_name_token decl = decl.module_type_name
  let name decl = Token.text decl.module_type_name
end

module OpenStatement = struct
  type t = {
    syntax_node : syntax_node;
    module_path : ModulePath.t;
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
  type_syntax_node : syntax_node;
}

type external_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_syntax_node : syntax_node;
  primitive_name_tokens : Token.t list;
}

type include_statement = {
  syntax_node : syntax_node;
  included_syntax_node : syntax_node;
}

type exception_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

module Item = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | Expression of Expression.t
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | ValueDeclaration of value_declaration
    | ExternalDeclaration of external_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration
    | Unknown of syntax_node

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | Expression expr -> Expression.syntax_node expr
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | ValueDeclaration decl -> decl.syntax_node
    | ExternalDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
    | Unknown node -> node
end

module SourceFile = struct
  type t = {
    syntax_node : syntax_node;
    items : Item.t list;
    let_bindings : LetBinding.t list;
    expressions : Expression.t list;
  }

  let syntax_node source_file = source_file.syntax_node
  let items source_file = source_file.items
  let let_bindings source_file = source_file.let_bindings
  let expressions source_file = source_file.expressions
end

type source_file = SourceFile.t

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
