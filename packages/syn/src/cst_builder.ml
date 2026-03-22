open Std
open Std.Collections

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

exception Bail of error

let bail ~message ~syntax_node ~context =
  raise
    (Bail
       {
         message;
         syntax_kind = Ceibo.Red.SyntaxNode.kind syntax_node;
         span = Ceibo.Red.SyntaxNode.span syntax_node;
         context;
       })

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let token syntax_tok = Cst.Token.{ syntax_token = syntax_tok }

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
  Cst.ModulePath.{ syntax_node = node; segments = parts }

let ident_path_from_node node =
  let parts =
    match direct_non_trivia_tokens node with
    | first :: _ -> [ token first ]
    | [] -> []
  in
  Cst.ModulePath.{ syntax_node = node; segments = parts }

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
          Cst.Parameter.Labeled
            Cst.LabeledParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
              }
      | None -> Cst.Parameter.Unknown node)
  | Syntax_kind.OPTIONAL_PARAM -> (
      match first_ident_token_in_subtree node with
      | Some label_name_token ->
          Cst.Parameter.Optional
            Cst.OptionalParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
                has_default = false;
              }
      | None -> Cst.Parameter.Unknown node)
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> (
      match direct_non_trivia_nodes node |> List.find_map first_ident_token_in_subtree with
      | Some label_name_token ->
          Cst.Parameter.Optional
            Cst.OptionalParameter.
              {
                syntax_node = node;
                label_token = label_name_token;
                binding_name_token = None;
                has_default = true;
              }
      | None -> Cst.Parameter.Unknown node)
  | Syntax_kind.TYPE_CONSTRAINT ->
      Cst.Parameter.LocallyAbstract node
  | _ ->
      Cst.Parameter.Positional
        Cst.PositionalParameter.
          {
            syntax_node = node;
            name_token = simple_pattern_name_token node;
          }

let rec pattern_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          Cst.Pattern.Identifier { syntax_node = node; name_token = token first }
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.WILDCARD_PATTERN ->
      Cst.Pattern.Wildcard { syntax_node = node }
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Bool
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Pattern.Literal (Cst.PatternLiteral.Unit { syntax_node = node })
  | Syntax_kind.PAREN_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Parenthesized
            { syntax_node = node; inner = pattern_from_node inner_node }
      | [] -> Cst.Pattern.Unknown node)
  | _ -> Cst.Pattern.Unknown node

let rec expression_from_node node =
  let known_expression_children node =
    direct_non_trivia_nodes node
    |> List.filter_map (fun child ->
           match expression_from_node child with
           | Cst.Expression.Unknown _ -> None
           | expr -> Some expr)
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.Expression.Path
        { syntax_node = node; path = ident_path_from_node node }
  | Syntax_kind.MODULE_PATH ->
      Cst.Expression.Path
        { syntax_node = node; path = module_path_from_node node }
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Expression.Literal (Cst.Literal.Unit { syntax_node = node })
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match field_access_expression_from_node node with
      | Some expr -> expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Expression.Unknown node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Expression.Unknown node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Bool
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Expression.Unknown node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Cst.Expression.Apply
            {
              syntax_node = node;
              callee = expression_from_node callee_node;
              argument = expression_from_node argument_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.INFIX_EXPR -> (
      match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
      | left_node :: right_node :: _, operator_syntax_token :: _ ->
          Cst.Expression.Infix
            {
              syntax_node = node;
              left = expression_from_node left_node;
              operator_token = token operator_syntax_token;
              right = expression_from_node right_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.TUPLE_EXPR ->
      Cst.Expression.Tuple { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.LIST_EXPR ->
      Cst.Expression.List { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.ARRAY_EXPR ->
      Cst.Expression.Array { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.RECORD_EXPR ->
      Cst.Expression.Record { syntax_node = node }
  | Syntax_kind.LOCAL_OPEN_EXPR -> (
      match local_open_expression_from_node node with
      | Some expr -> Cst.Expression.LocalOpen expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.FUN_EXPR -> (
      match fun_expression_from_node node with
      | Some expr -> Cst.Expression.Fun expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.FUNCTION_EXPR -> (
      match function_expression_from_node node with
      | Some expr -> Cst.Expression.Function expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.LET_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:false node with
      | Some expr -> Cst.Expression.Let expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.LET_REC_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.Expression.Let expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.MATCH_EXPR -> (
      match match_expression_from_node node with
      | Some expr -> Cst.Expression.Match expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.TRY_EXPR -> (
      match try_expression_from_node node with
      | Some expr -> Cst.Expression.Try expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.IF_EXPR -> (
      let expression_children =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               match expression_from_node child with
               | Cst.Expression.Unknown _ -> false
               | _ -> true)
      in
      match expression_children with
      | condition_node :: then_node :: else_nodes ->
          Cst.Expression.If
            {
              syntax_node = node;
              condition = expression_from_node condition_node;
              then_branch = expression_from_node then_node;
              else_branch =
                (match else_nodes with
                | else_node :: _ -> Some (expression_from_node else_node)
                | [] -> None);
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Expression.Parenthesized
            { syntax_node = node; inner = expression_from_node inner_node }
      | [] -> Cst.Expression.Unknown node)
  | _ -> Cst.Expression.Unknown node

and field_access_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, field_token :: _ ->
      Some
        (Cst.Expression.FieldAccess
           {
             syntax_node = node;
             receiver = expression_from_node receiver_node;
             field_name = token field_token;
           })
  | _ -> None

and local_open_expression_from_node node =
  let module_path_of_scope_node child =
    match Ceibo.Red.SyntaxNode.kind child with
    | Syntax_kind.MODULE_PATH -> Some (module_path_from_node child)
    | Syntax_kind.IDENT_EXPR -> Some (ident_path_from_node child)
    | _ -> None
  in
  let non_trivia_children = direct_non_trivia_nodes node in
  let via_let_open =
    direct_non_trivia_tokens node
    |> List.exists (fun tok ->
           String.equal (Ceibo.Red.SyntaxToken.text tok) "let")
  in
  let body_expr =
    List.rev non_trivia_children
    |> List.find_map (fun child ->
           match expression_from_node child with
           | Cst.Expression.Unknown _ -> None
           | expr -> Some expr)
  in
  match List.find_map module_path_of_scope_node non_trivia_children, body_expr with
  | Some lifted_module_path, Some lifted_body ->
      Some
        {
          syntax_node = node;
          module_path = lifted_module_path;
          body = lifted_body;
          via_let_open;
        }
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
        Cst.FunExpression.
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
  Some Cst.FunctionExpression.{ syntax_node = node; cases = match_cases }

and let_expression_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
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
            Cst.LetExpression.
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
           | Cst.Expression.Unknown _ -> None
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
                Cst.MatchCase.
                  {
                    syntax_node = node;
                    pattern = pattern_from_node pattern_node;
                    guard = None;
                    body = body_expr;
                  }
          | [] -> None)
      | guard_expr :: body_expr :: _, true ->
          Some
            Cst.MatchCase.
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                guard = Some guard_expr;
                body = body_expr;
              }
      | body_expr :: _, true ->
          Some
            Cst.MatchCase.
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                guard = None;
                body = body_expr;
              })
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
        Cst.MatchExpression.
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
        Cst.TryExpression.
          {
            syntax_node = node;
            body = expression_from_node body_node;
            cases = match_cases;
          }
  | [] -> None

let type_variable_from_node node =
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ ->
      Some Cst.TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

let type_parameter_from_node node =
  let lifted_type_variable =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  Cst.TypeParameter.{ syntax_node = node; type_variable = lifted_type_variable }

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
         Cst.RecordField.
           {
             syntax_node = node;
             field_name = field_name;
             is_mutable = mutable_field;
           })

let variant_constructor_from_node node =
  match direct_non_trivia_nodes node with
  | first_child :: _ -> (
      match direct_non_trivia_tokens first_child with
      | constructor_name :: _ ->
          Some
            Cst.VariantConstructor.
              { syntax_node = node; constructor_name = token constructor_name }
      | [] -> None)
  | [] -> None

let poly_variant_tag_from_node node =
  match direct_non_trivia_tokens node with
  | _backtick :: tag_name :: _ ->
      Some Cst.PolyVariantTag.{ syntax_node = node; tag_name = token tag_name }
  | tag_name :: _ ->
      Some Cst.PolyVariantTag.{ syntax_node = node; tag_name = token tag_name }
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
         | _ -> Cst.ModulePath.{ syntax_node = child; segments = [] })

let type_definition_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let variant_constructors =
    direct_children
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  if List.length variant_constructors > 0 then
    Cst.TypeDefinition.Variant variant_constructors
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
        Cst.TypeDefinition.Record fields
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
            Cst.TypeDefinition.PolyVariant tags
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
            | [] -> Cst.TypeDefinition.Abstract
            | first :: _ ->
                let kind = Ceibo.Red.SyntaxNode.kind first in
                if kind = Syntax_kind.TYPE_CONSTR || kind = Syntax_kind.TYPE_ARROW
                   || kind = Syntax_kind.TYPE_TUPLE || kind = Syntax_kind.TYPE_VAR
                   || kind = Syntax_kind.TYPE_ALIAS
                then
                  Cst.TypeDefinition.Alias { syntax_node = first }
                else Cst.TypeDefinition.Other first)

let type_declaration_from_node node =
  let lifted_type_params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  match type_declaration_name_path node with
  | Some lifted_type_name -> (
      match Cst.ModulePath.last_segment lifted_type_name with
      | Some _ ->
          Some
            Cst.TypeDeclaration.
              {
                syntax_node = node;
                type_name = lifted_type_name;
                type_params = lifted_type_params;
                type_definition = type_definition_from_node node;
              }
      | None -> None)
  | None -> None

let let_binding_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
         (direct_non_trivia_tokens node)
  in
  match direct_non_trivia_nodes node with
  | name_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_params
        when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_PATTERN ->
          name_token_from_ident_pattern name_node
          |> Option.map (fun binding_name ->
                 Cst.LetBinding.
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
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
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
    | child :: rest when is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child) ->
        split_parameters (child :: params) rest
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT ->
        split_parameters params rest
    | child :: _ -> Some (List.rev params, child)
    | [] -> None
  in
  match find_name_node (direct_non_trivia_nodes node) with
  | Some (name_node, rest) -> (
      match name_token_from_ident_pattern name_node, split_parameters [] rest with
      | Some binding_name, Some (param_nodes, bound_value_node) ->
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                binding_name;
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
    Cst.ModuleDeclaration.
      { syntax_node = node; module_name = token module_name }
  | _ -> None

let module_type_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: _type_kw :: module_type_name :: _ ->
      Some
        Cst.ModuleTypeDeclaration.
          { syntax_node = node; module_type_name = token module_type_name }
  | _ -> None

let open_statement_from_node node =
  let tokens = direct_non_trivia_tokens node in
  let bang_token_opt =
    tokens
    |> List.find_opt (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "!")
    |> Option.map token
  in
  let module_segments =
    tokens
    |> List.filter (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
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
        Cst.OpenStatement.
          {
            syntax_node = node;
            module_path = Cst.ModulePath.{ syntax_node = node; segments = module_segments };
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
    | Cst.Expression.Unknown _ -> []
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
      | Some decl -> [ Cst.Item.TypeDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      direct_non_trivia_nodes node |> List.concat_map items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Cst.Item.LetBinding binding ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Cst.Item.LetBinding binding ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.LET_MUTUAL_DECL ->
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      |> List.concat_map items_from_node
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl -> [ Cst.Item.ModuleDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.Item.ModuleTypeDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.Item.OpenStatement stmt ]
      | None -> [ Cst.Item.Unknown node ])
  | _ -> [ Cst.Item.Unknown node ]

let of_green_tree tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  let file_let_bindings = collect_let_bindings root in
  let file_expressions = collect_expressions root in
  Cst.SourceFile.
    {
      syntax_node = root;
      items = file_items;
      let_bindings = file_let_bindings;
      expressions = file_expressions;
    }

let rec validate_pattern ~context = function
  | Cst.Pattern.Identifier _ | Cst.Pattern.Wildcard _ | Cst.Pattern.Literal _ -> ()
  | Cst.Pattern.Parenthesized { inner; _ } ->
      validate_pattern ~context:("pattern.parenthesized" :: context) inner
  | Cst.Pattern.Unknown syntax_node ->
      bail ~message:"unsupported pattern shape during Ceibo -> CST lifting"
        ~syntax_node ~context

and validate_parameter ~context = function
  | Cst.Parameter.Positional _ | Cst.Parameter.Labeled _
  | Cst.Parameter.Optional _ | Cst.Parameter.LocallyAbstract _ ->
      ()
  | Cst.Parameter.Unknown syntax_node ->
      bail ~message:"unsupported parameter shape during Ceibo -> CST lifting"
        ~syntax_node ~context

and validate_expression ~context = function
  | Cst.Expression.Path _ | Cst.Expression.Literal _ | Cst.Expression.Record _ -> ()
  | Cst.Expression.Apply { callee; argument; _ } ->
      validate_expression ~context:("expression.apply.callee" :: context) callee;
      validate_expression ~context:("expression.apply.argument" :: context) argument
  | Cst.Expression.FieldAccess { receiver; _ } ->
      validate_expression ~context:("expression.field_access.receiver" :: context)
        receiver
  | Cst.Expression.Infix { left; right; _ } ->
      validate_expression ~context:("expression.infix.left" :: context) left;
      validate_expression ~context:("expression.infix.right" :: context) right
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      List.iteri
        (fun index expr ->
          validate_expression
            ~context:(("expression.element[" ^ Int.to_string index ^ "]") :: context)
            expr)
        elements
  | Cst.Expression.LocalOpen { body; _ } ->
      validate_expression ~context:("expression.local_open.body" :: context) body
  | Cst.Expression.Fun { parameters; body; _ } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:(("expression.fun.parameter[" ^ Int.to_string index ^ "]") :: context)
            parameter)
        parameters;
      validate_expression ~context:("expression.fun.body" :: context) body
  | Cst.Expression.Function { cases; _ } ->
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.function.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.Let { binding_pattern; bound_value; body; _ } ->
      validate_pattern ~context:("expression.let.pattern" :: context) binding_pattern;
      validate_expression ~context:("expression.let.bound_value" :: context) bound_value;
      validate_expression ~context:("expression.let.body" :: context) body
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      validate_expression ~context:("expression.match.scrutinee" :: context) scrutinee;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.match.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.Try { body; cases; _ } ->
      validate_expression ~context:("expression.try.body" :: context) body;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.try.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      validate_expression ~context:("expression.if.condition" :: context) condition;
      validate_expression ~context:("expression.if.then_branch" :: context) then_branch;
      Option.iter
        (validate_expression ~context:("expression.if.else_branch" :: context))
        else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      validate_expression ~context:("expression.parenthesized" :: context) inner
  | Cst.Expression.Unknown syntax_node ->
      bail ~message:"unsupported expression shape during Ceibo -> CST lifting"
        ~syntax_node ~context

and validate_match_case ~context ({ pattern; guard; body; _ } : Cst.match_case) =
  validate_pattern ~context:("match_case.pattern" :: context) pattern;
  Option.iter (validate_expression ~context:("match_case.guard" :: context)) guard;
  validate_expression ~context:("match_case.body" :: context) body

let validate_type_definition ~context = function
  | Cst.TypeDefinition.Abstract -> ()
  | Cst.TypeDefinition.Alias _ -> ()
  | Cst.TypeDefinition.Record _ -> ()
  | Cst.TypeDefinition.Variant _ -> ()
  | Cst.TypeDefinition.PolyVariant _ -> ()
  | Cst.TypeDefinition.Other syntax_node ->
      bail ~message:"unsupported type definition shape during Ceibo -> CST lifting"
        ~syntax_node ~context

let validate_item ~context = function
  | Cst.Item.TypeDeclaration { type_definition; _ } ->
      validate_type_definition ~context:("item.type_declaration" :: context)
        type_definition
  | Cst.Item.LetBinding { value; _ } ->
      validate_expression ~context:("item.let_binding.value" :: context) value
  | Cst.Item.ModuleDeclaration _ | Cst.Item.ModuleTypeDeclaration _
  | Cst.Item.OpenStatement _ ->
      ()
  | Cst.Item.Unknown syntax_node ->
      bail ~message:"unsupported structure item during Ceibo -> CST lifting"
        ~syntax_node ~context

let validate_source_file ({ items; let_bindings; expressions; _ } : Cst.source_file) =
  List.iteri
    (fun index item ->
      validate_item ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ] item)
    items;
  List.iteri
    (fun index binding ->
      validate_expression
        ~context:[ "source_file.let_bindings[" ^ Int.to_string index ^ "].value" ]
        (Cst.LetBinding.value binding))
    let_bindings;
  List.iteri
    (fun index expr ->
      validate_expression
        ~context:[ "source_file.expressions[" ^ Int.to_string index ^ "]" ]
        expr)
    expressions

let lift tree =
  let cst = of_green_tree tree in
  validate_source_file cst;
  cst

let create_from_ceibo tree =
  match lift tree with
  | cst -> Ok cst
  | exception Bail error -> Error error
