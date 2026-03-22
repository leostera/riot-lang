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
  | FieldAccess of field_access_expression
  | Infix of infix_expression
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

and apply_expression = {
  syntax_node : syntax_node;
  callee : expression;
  argument : expression;
}

and field_access_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  field_name : Token.t;
}

and infix_expression = {
  syntax_node : syntax_node;
  left : expression;
  operator_token : Token.t;
  right : expression;
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
    | Apply of apply_expression
    | FieldAccess of field_access_expression
    | Infix of infix_expression
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
        | Literal.Bool { syntax_node; _ }
        | Literal.Unit { syntax_node } ->
            syntax_node)
    | Apply expr -> expr.syntax_node
    | FieldAccess expr -> expr.syntax_node
    | Infix expr -> expr.syntax_node
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
    | Parenthesized of parenthesized_pattern
    | Unknown of syntax_node

  let syntax_node = function
    | Identifier pattern -> pattern.syntax_node
    | Wildcard pattern -> pattern.syntax_node
    | Literal (PatternLiteral.String { syntax_node; _ })
    | Literal (PatternLiteral.Int { syntax_node; _ })
    | Literal (PatternLiteral.Bool { syntax_node; _ })
    | Literal (PatternLiteral.Unit { syntax_node }) ->
        syntax_node
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

module ParenthesizedPattern = struct
  type t = parenthesized_pattern = {
    syntax_node : syntax_node;
    inner : pattern;
  }

  let syntax_node pattern = pattern.syntax_node
  let inner pattern = pattern.inner
end

module PathExpression = struct
  type t = path_expression = {
    syntax_node : syntax_node;
    path : ModulePath.t;
  }

  let syntax_node expr = expr.syntax_node
  let path expr = expr.path
end


module ApplyExpression = struct
  type t = apply_expression = {
    syntax_node : syntax_node;
    callee : expression;
    argument : expression;
  }

  let syntax_node expr = expr.syntax_node
  let callee expr = expr.callee
  let argument expr = expr.argument
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
    binding_name : Token.t;
    parameters : Parameter.t list;
    value : Expression.t;
    is_recursive : bool;
  }

  let syntax_node binding = binding.syntax_node
  let binding_name_token binding = binding.binding_name
  let name binding = Token.text binding.binding_name
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

module Item = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | LetBinding of LetBinding.t
    | ModuleDeclaration of ModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | Unknown of syntax_node

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
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

let token token = Token.{ syntax_token = token }

let direct_non_trivia_nodes node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Node child -> Some child
       | Ceibo.Red.Token tok
         when is_trivia (Ceibo.Red.SyntaxToken.kind tok) ->
           None
       | Ceibo.Red.Token _ -> None)

let direct_non_trivia_tokens node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Token tok
         when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
           Some tok
       | _ -> None)

let module_path_from_node node =
  let parts = direct_non_trivia_tokens node |> List.map token in
  ModulePath.{ syntax_node = node; segments = parts }

let ident_path_from_node node =
  let parts =
    match direct_non_trivia_tokens node with
    | first :: _ -> [ token first ]
    | [] -> []
  in
  ModulePath.{ syntax_node = node; segments = parts }

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT ->
      true
  | _ -> false

let name_token_from_ident_pattern node =
  match direct_non_trivia_tokens node with
  | first :: _ -> Some (token first)
  | [] -> None

let is_identifier_like_text text =
  String.length text > 0
  &&
  let ch = String.get text 0 in
  (ch >= 'a' && ch <= 'z')
  || (ch >= 'A' && ch <= 'Z')
  || ch = '_'

let rec simple_pattern_name_token node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN ->
      name_token_from_ident_pattern node
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.LAZY_PATTERN ->
      (match direct_non_trivia_nodes node |> List.find_opt (fun _ -> true) with
      | Some child -> simple_pattern_name_token child
      | None -> None)
  | Syntax_kind.AS_PATTERN ->
      (match
         direct_non_trivia_nodes node
         |> List.rev
         |> List.find_opt (fun child ->
                Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN)
       with
      | Some child -> name_token_from_ident_pattern child
      | None -> None)
  | _ -> None

let first_ident_token_in_subtree node =
  let rec go_node node =
    match
      direct_non_trivia_tokens node
      |> List.find_opt (fun tok ->
             is_identifier_like_text (Ceibo.Red.SyntaxToken.text tok))
    with
    | Some tok -> Some (token tok)
    | None -> direct_non_trivia_nodes node |> List.find_map go_node
  in
  go_node node

let parameter_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_PARAM -> (
      match first_ident_token_in_subtree node with
      | Some label_name_token ->
          Parameter.Labeled
            LabeledParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
              }
      | None -> Parameter.Unknown node)
  | Syntax_kind.OPTIONAL_PARAM -> (
      match first_ident_token_in_subtree node with
      | Some label_name_token ->
          Parameter.Optional
            OptionalParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
                has_default = false;
              }
      | None -> Parameter.Unknown node)
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> (
      match direct_non_trivia_nodes node |> List.find_map first_ident_token_in_subtree with
      | Some label_name_token ->
          Parameter.Optional
            OptionalParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
                has_default = true;
              }
      | None -> Parameter.Unknown node)
  | Syntax_kind.TYPE_CONSTRAINT ->
      Parameter.LocallyAbstract node
  | _ ->
      Parameter.Positional
        PositionalParameter.
          {
            syntax_node = node;
            name_token = simple_pattern_name_token node;
          }

let rec pattern_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          Pattern.Identifier
            IdentifierPattern.{ syntax_node = node; name_token = token first }
      | [] -> Pattern.Unknown node)
  | Syntax_kind.WILDCARD_PATTERN ->
      Pattern.Wildcard WildcardPattern.{ syntax_node = node }
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Pattern.Literal
            (PatternLiteral.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Pattern.Unknown node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Pattern.Literal
            (PatternLiteral.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Pattern.Unknown node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Pattern.Literal
            (PatternLiteral.Bool
              {
                syntax_node = node;
                literal_token = token literal_syntax_token;
              })
      | [] -> Pattern.Unknown node)
  | Syntax_kind.UNIT_LITERAL ->
      Pattern.Literal (PatternLiteral.Unit { syntax_node = node })
  | Syntax_kind.PAREN_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Pattern.Parenthesized
            ParenthesizedPattern.
              { syntax_node = node; inner = pattern_from_node inner_node }
      | [] -> Pattern.Unknown node)
  | _ -> Pattern.Unknown node

let rec expression_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Expression.Path
        PathExpression.
          { syntax_node = node; path = ident_path_from_node node }
  | Syntax_kind.MODULE_PATH ->
      Expression.Path
        PathExpression.
          { syntax_node = node; path = module_path_from_node node }
  | Syntax_kind.UNIT_LITERAL ->
      Expression.Literal (Literal.Unit { syntax_node = node })
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match field_access_expression_from_node node with
      | Some expr -> expr
      | None -> Expression.Unknown node)
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Expression.Literal
            (Literal.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Expression.Unknown node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Expression.Literal
            (Literal.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Expression.Unknown node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Expression.Literal
            (Literal.Bool
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Expression.Unknown node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Expression.Apply
            ApplyExpression.
              {
                syntax_node = node;
                callee = expression_from_node callee_node;
                argument = expression_from_node argument_node;
              }
      | _ -> Expression.Unknown node)
  | Syntax_kind.INFIX_EXPR -> (
      match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
      | left_node :: right_node :: _, operator_syntax_token :: _ ->
          Expression.Infix
            InfixExpression.
              {
                syntax_node = node;
                left = expression_from_node left_node;
                operator_token = token operator_syntax_token;
                right = expression_from_node right_node;
              }
      | _ -> Expression.Unknown node)
  | Syntax_kind.FUN_EXPR -> (
      match fun_expression_from_node node with
      | Some expr -> Expression.Fun expr
      | None -> Expression.Unknown node)
  | Syntax_kind.FUNCTION_EXPR -> (
      match function_expression_from_node node with
      | Some expr -> Expression.Function expr
      | None -> Expression.Unknown node)
  | Syntax_kind.LET_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:false node with
      | Some expr -> Expression.Let expr
      | None -> Expression.Unknown node)
  | Syntax_kind.LET_REC_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Expression.Let expr
      | None -> Expression.Unknown node)
  | Syntax_kind.MATCH_EXPR -> (
      match match_expression_from_node node with
      | Some expr -> Expression.Match expr
      | None -> Expression.Unknown node)
  | Syntax_kind.TRY_EXPR -> (
      match try_expression_from_node node with
      | Some expr -> Expression.Try expr
      | None -> Expression.Unknown node)
  | Syntax_kind.IF_EXPR -> (
      let expression_children =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               match expression_from_node child with
               | Expression.Unknown _ -> false
               | _ -> true)
      in
      match expression_children with
      | condition_node :: then_node :: else_nodes ->
          Expression.If
            IfExpression.
              {
                syntax_node = node;
                condition = expression_from_node condition_node;
                then_branch = expression_from_node then_node;
                else_branch =
                  (match else_nodes with
                  | else_node :: _ -> Some (expression_from_node else_node)
                  | [] -> None);
              }
      | _ -> Expression.Unknown node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Expression.Parenthesized
            ParenthesizedExpression.
              {
                syntax_node = node;
                inner = expression_from_node inner_node;
              }
      | [] -> Expression.Unknown node)
  | _ -> Expression.Unknown node

and field_access_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, field_token :: _ ->
      Some
        (Expression.FieldAccess
           {
             syntax_node = node;
             receiver = expression_from_node receiver_node;
             field_name = token field_token;
           })
  | _ -> None

and fun_expression_from_node node =
  let rec split_parameters params = function
    | child :: rest when is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child) ->
        split_parameters (child :: params) rest
    | body_node :: _ -> Some (List.rev params, body_node)
    | [] -> None
  in
  match split_parameters [] (direct_non_trivia_nodes node) with
  | Some (param_nodes, body_node) ->
      Some
        FunExpression.
          {
            syntax_node = node;
            parameters = List.map parameter_from_node param_nodes;
            body = expression_from_node body_node;
          }
  | None -> None

and function_expression_from_node node =
  let match_cases =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
    |> List.filter_map match_case_from_node
  in
  Some FunctionExpression.{ syntax_node = node; cases = match_cases }

and let_expression_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun token -> String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
         (direct_non_trivia_tokens node)
  in
  let rec split_parameters params = function
    | child :: rest when is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child) ->
        split_parameters (child :: params) rest
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT ->
        split_parameters params rest
    | bound_value_node :: body_node :: _ ->
        Some (List.rev params, bound_value_node, body_node)
    | _ -> None
  in
  match direct_non_trivia_nodes node with
  | binding_pattern_node :: rest -> (
      match split_parameters [] rest with
      | Some (_params, bound_value_node, body_node) ->
          Some
            LetExpression.
              {
                syntax_node = node;
                binding_pattern = pattern_from_node binding_pattern_node;
                bound_value = expression_from_node bound_value_node;
                body = expression_from_node body_node;
                is_recursive = is_recursive_binding;
              }
      | None -> None)
  | [] -> None

and match_case_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let expression_children =
    non_trivia_children
    |> List.filter_map (fun child ->
           match expression_from_node child with
           | Expression.Unknown _ -> None
           | expr -> Some expr)
  in
  let has_guard =
    List.exists
      (fun child ->
        Ceibo.Red.SyntaxNode.kind child = Syntax_kind.PATTERN_GUARD)
      non_trivia_children
  in
  match non_trivia_children with
  | pattern_node :: _ -> (
      match expression_children, has_guard with
      | [], _ -> None
      | body_exprs, false -> (
          match List.rev body_exprs with
          | body_expr :: _ ->
              Some
                MatchCase.
                  {
                    syntax_node = node;
                    pattern = pattern_from_node pattern_node;
                    guard = None;
                    body = body_expr;
                  }
          | [] -> None)
      | guard_expr :: body_expr :: _, true ->
          Some
            MatchCase.
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                guard = Some guard_expr;
                body = body_expr;
              }
      | body_expr :: _, true ->
          Some
            MatchCase.
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                guard = None;
                body = body_expr;
              }
      )
  | [] -> None

and match_expression_from_node node =
  match direct_non_trivia_nodes node with
  | scrutinee_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        MatchExpression.
          {
            syntax_node = node;
            scrutinee = expression_from_node scrutinee_node;
            cases = match_cases;
          }
  | [] -> None

and try_expression_from_node node =
  match direct_non_trivia_nodes node with
  | body_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        TryExpression.
          {
            syntax_node = node;
            body = expression_from_node body_node;
            cases = match_cases;
          }
  | [] -> None

let type_variable_from_node node =
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ ->
      Some TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

let type_parameter_from_node node =
  let type_var =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  TypeParameter.{ syntax_node = node; type_variable = type_var }

let record_field_name_token node =
  match direct_non_trivia_tokens node with
  | mutable_kw :: field_name :: _
    when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" ->
      Some (token field_name)
  | field_name :: _ -> Some (token field_name)
  | [] -> None

let record_field_from_node node =
  record_field_name_token node
  |> Option.map (fun field_name ->
         let mutable_field =
           match direct_non_trivia_tokens node with
           | first :: _ ->
               String.equal (Ceibo.Red.SyntaxToken.text first) "mutable"
           | [] -> false
         in
         RecordField.{ syntax_node = node; field_name; is_mutable = mutable_field })

let variant_constructor_from_node node =
  match direct_non_trivia_nodes node with
  | first_child :: _ -> (
      match direct_non_trivia_tokens first_child with
      | constructor_name :: _ ->
          Some
            VariantConstructor.
              { syntax_node = node; constructor_name = token constructor_name }
      | [] -> None)
  | [] -> None

let poly_variant_tag_from_node node =
  match direct_non_trivia_tokens node with
  | _backtick :: tag_name :: _ ->
      Some PolyVariantTag.{ syntax_node = node; tag_name = token tag_name }
  | tag_name :: _ ->
      Some PolyVariantTag.{ syntax_node = node; tag_name = token tag_name }
  | [] -> None

let type_declaration_name_path node =
  let is_name_node child =
    let kind = Ceibo.Red.SyntaxNode.kind child in
    kind = Syntax_kind.IDENT_EXPR || kind = Syntax_kind.MODULE_PATH
  in
  direct_non_trivia_nodes node
  |> List.find_opt is_name_node
  |> Option.map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.MODULE_PATH -> module_path_from_node child
         | Syntax_kind.IDENT_EXPR -> ident_path_from_node child
         | _ -> ModulePath.{ syntax_node = child; segments = [] })

let type_definition_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let variant_constructors =
    direct_children
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  if List.length variant_constructors > 0 then
    TypeDefinition.Variant variant_constructors
  else
    match
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD)
    with
    | Some record_node ->
        let fields =
          direct_non_trivia_nodes record_node
          |> List.filter (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
          |> List.filter_map record_field_from_node
        in
        TypeDefinition.Record fields
    | None -> (
        match
          direct_children
          |> List.find_opt (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_POLY_VARIANT)
        with
        | Some poly_variant_node ->
            let tags =
              direct_non_trivia_nodes poly_variant_node
              |> List.filter (fun child ->
                     Ceibo.Red.SyntaxNode.kind child = Syntax_kind.POLY_VARIANT_TAG)
              |> List.filter_map poly_variant_tag_from_node
            in
            TypeDefinition.PolyVariant tags
        | None ->
            let remaining_nodes =
              direct_children
              |> List.filter (fun child ->
                     let kind = Ceibo.Red.SyntaxNode.kind child in
                     kind != Syntax_kind.TYPE_PARAM
                     && kind != Syntax_kind.IDENT_EXPR
                     && kind != Syntax_kind.MODULE_PATH)
            in
            match remaining_nodes with
            | [] -> TypeDefinition.Abstract
            | first :: _ -> TypeDefinition.Other first)

let type_declaration_from_node node =
  let params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  match type_declaration_name_path node with
  | Some path -> (
      match ModulePath.last_segment path with
      | Some _ ->
          Some
            TypeDeclaration.
              {
                syntax_node = node;
                type_name = path;
                type_params = params;
                type_definition = type_definition_from_node node;
              }
      | None -> None)
  | None -> None

let let_binding_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun token -> String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
         (direct_non_trivia_tokens node)
  in
  match direct_non_trivia_nodes node with
  | name_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_params
        when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_PATTERN ->
          name_token_from_ident_pattern name_node
          |> Option.map (fun binding_name ->
                 LetBinding.
                   {
                     syntax_node = node;
                     binding_name;
                     parameters = List.rev rev_params |> List.map parameter_from_node;
                     value = expression_from_node value_node;
                     is_recursive = is_recursive_binding;
                   })
      | _ -> None)
  | [] -> None

let let_expression_binding_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun token -> String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
         (direct_non_trivia_tokens node)
  in
  let rec find_name_node = function
    | [] -> None
    | child :: rest ->
        if Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN then
          Some (child, rest)
        else
          find_name_node rest
  in
  let rec split_parameters params = function
    | child :: rest when is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child)
      ->
        split_parameters (child :: params) rest
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT
      ->
        split_parameters params rest
    | child :: _ -> Some (List.rev params, child)
    | [] -> None
  in
  match find_name_node (direct_non_trivia_nodes node) with
  | Some (name_node, rest) -> (
      match name_token_from_ident_pattern name_node, split_parameters [] rest with
      | Some binding_name, Some (param_nodes, bound_value_node) ->
          Some LetBinding.
                 {
                   syntax_node = node;
                   binding_name = binding_name;
                   parameters = List.map parameter_from_node param_nodes;
                   value = expression_from_node bound_value_node;
                   is_recursive = is_recursive_binding;
                 }
      | _ -> None)
  | None -> None

let module_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: module_name :: _ ->
      Some
        ModuleDeclaration.
          {
            syntax_node = node;
            module_name = token module_name;
          }
  | _ -> None

let module_type_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: _type_kw :: module_type_name :: _ ->
      Some ModuleTypeDeclaration.
             { syntax_node = node; module_type_name = token module_type_name }
  | _ -> None

let open_statement_from_node node =
  let tokens = direct_non_trivia_tokens node in
  let bang_token_opt =
    tokens
    |> List.find_opt (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
    |> Option.map token
  in
  let module_segments =
    tokens
    |> List.filter (fun tok ->
           let text = Ceibo.Red.SyntaxToken.text tok in
           not
             (String.equal text "open"
             || String.equal text "!"
             || String.equal text "."))
    |> List.map token
  in
  match module_segments with
  | [] -> None
  | _ ->
      Some
        OpenStatement.
          {
            syntax_node = node;
            module_path = ModulePath.{ syntax_node = node; segments = module_segments };
            bang_token = bang_token_opt;
          }

let rec collect_let_bindings node =
  let bindings_here =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.LET_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:false node)
    | Syntax_kind.LET_REC_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:true node)
    | Syntax_kind.LET_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:false node)
    | Syntax_kind.LET_REC_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:true node)
    | _ -> []
  in
  let nested =
    direct_non_trivia_nodes node |> List.concat_map collect_let_bindings
  in
  bindings_here @ nested

let rec collect_expressions node =
  let expressions_here =
    match expression_from_node node with
    | Expression.Unknown _ -> []
    | expr -> [ expr ]
  in
  let nested =
    direct_non_trivia_nodes node |> List.concat_map collect_expressions
  in
  expressions_here @ nested

let rec items_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      match type_declaration_from_node node with
      | Some decl -> [ Item.TypeDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      direct_non_trivia_nodes node |> List.concat_map items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Item.LetBinding binding ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Item.LetBinding binding ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.LET_MUTUAL_DECL ->
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      |> List.concat_map items_from_node
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl -> [ Item.ModuleDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Item.ModuleTypeDeclaration decl ]
      | None -> [ Item.Unknown node ])
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Item.OpenStatement stmt ]
      | None -> [ Item.Unknown node ])
  | _ -> [ Item.Unknown node ]

let of_green_tree tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  let file_let_bindings = collect_let_bindings root in
  let file_expressions = collect_expressions root in
  SourceFile.
    {
      syntax_node = root;
      items = file_items;
      let_bindings = file_let_bindings;
      expressions = file_expressions;
    }

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
