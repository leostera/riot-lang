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

let module_path_like_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH | Syntax_kind.MODULE_TYPE_PATH ->
      module_path_from_node node
  | Syntax_kind.IDENT_EXPR ->
      ident_path_from_node node
  | _ ->
      Cst.ModulePath.{ syntax_node = node; segments = List.map token (direct_non_trivia_tokens node) }

let attribute_from_node node : Cst.attribute =
  { Cst.syntax_node = node; tokens = List.map token (direct_non_trivia_tokens node) }

let extension_from_node node : Cst.extension =
  { Cst.syntax_node = node; tokens = List.map token (direct_non_trivia_tokens node) }

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
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

let is_attribute_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.ATTRIBUTE_EXPR

let is_let_binding_node node =
  let kind = Ceibo.Red.SyntaxNode.kind node in
  kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING

let split_at_first_and_binding nodes =
  let rec loop acc = function
    | child :: rest when is_let_binding_node child ->
        (List.rev acc, child :: rest)
    | child :: rest ->
        loop (child :: acc) rest
    | [] -> (List.rev acc, [])
  in
  loop [] nodes

let let_expression_parts ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
         (direct_non_trivia_tokens node)
  in
  let binding_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | exception_decl :: rest
    when Ceibo.Red.SyntaxNode.kind exception_decl = Syntax_kind.EXCEPTION_DECL ->
      (match List.rev rest with
      | body_node :: _ ->
          Some (`Exception (exception_decl, body_node))
      | [] -> None)
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | body_node :: rev_prefix ->
          let prefix = List.rev rev_prefix in
          let binding_prefix, and_binding_nodes =
            split_at_first_and_binding prefix
          in
          (match List.rev binding_prefix with
          | bound_value_node :: rev_param_nodes ->
              Some
                (`Value
                  ( is_recursive_binding,
                    binding_pattern_node,
                    List.rev rev_param_nodes,
                    bound_value_node,
                    and_binding_nodes,
                    body_node ))
          | [] -> None)
      | [] -> None)
  | [] -> None

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

let rec take_tokens_until_equals acc = function
  | [] -> List.rev acc
  | syntax_token :: rest ->
      if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=" then
        List.rev acc
      else
        take_tokens_until_equals (syntax_token :: acc) rest

let rec pattern_from_node node =
  let pattern_children node = direct_non_trivia_nodes node |> List.map pattern_from_node in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          Cst.Pattern.Identifier { syntax_node = node; name_token = token first }
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.WILDCARD_PATTERN ->
      Cst.Pattern.Wildcard { syntax_node = node }
  | Syntax_kind.LAZY_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Lazy
            {
              syntax_node = node;
              pattern = pattern_from_node inner_node;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.EXCEPTION_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Exception
            {
              syntax_node = node;
              pattern = pattern_from_node inner_node;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.RANGE_PATTERN -> (
      match direct_non_trivia_tokens node with
      | lower_syntax_token :: _range_syntax_token :: upper_syntax_token :: _ ->
          Cst.Pattern.Range
            {
              syntax_node = node;
              lower_token = token lower_syntax_token;
              upper_token = token upper_syntax_token;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN -> (
      match direct_non_trivia_tokens node with
      | _lparen :: _module_kw :: name_syntax_token :: _ ->
          Cst.Pattern.FirstClassModule
            {
              syntax_node = node;
              name_token = token name_syntax_token;
              module_type_syntax_node =
                (direct_non_trivia_nodes node
                |> List.find_opt (fun child ->
                       let kind = Ceibo.Red.SyntaxNode.kind child in
                       kind = Syntax_kind.MODULE_TYPE_PATH
                       || kind = Syntax_kind.MODULE_TYPE_OF));
            }
      | _ -> Cst.Pattern.Unknown node)
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
  | Syntax_kind.FLOAT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Float
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Pattern.Unknown node)
  | Syntax_kind.CHAR_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Char
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
  | Syntax_kind.POLY_VARIANT_PATTERN -> (
      match poly_variant_tag_token node with
      | Some tag_token ->
          Cst.Pattern.PolyVariant
            {
              syntax_node = node;
              tag_token;
              payload =
                (direct_non_trivia_nodes node
                |> List.find_map (fun child ->
                       match pattern_from_node child with
                       | Cst.Pattern.Unknown _ -> None
                       | pattern -> Some pattern));
            }
      | None -> Cst.Pattern.Unknown node)
  | Syntax_kind.CONSTRUCTOR_PATTERN ->
      Cst.Pattern.Constructor
        {
          syntax_node = node;
          constructor_path = module_path_from_node node;
          arguments = pattern_children node;
        }
  | Syntax_kind.TUPLE_PATTERN ->
      Cst.Pattern.Tuple { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.LIST_PATTERN ->
      Cst.Pattern.List { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.ARRAY_PATTERN ->
      Cst.Pattern.Array { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.RECORD_PATTERN ->
      Cst.Pattern.Record
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD_PATTERN)
            |> List.filter_map record_pattern_field_from_node;
        }
  | Syntax_kind.CONS_PATTERN -> (
      match direct_non_trivia_nodes node with
      | head_node :: tail_node :: _ ->
          Cst.Pattern.Cons
            {
              syntax_node = node;
              head = pattern_from_node head_node;
              tail = pattern_from_node tail_node;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.OR_PATTERN ->
      Cst.Pattern.Or
        { syntax_node = node; alternatives = pattern_children node }
  | Syntax_kind.AS_PATTERN -> (
      match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
      | pattern_node :: _, name_syntax_token :: _ ->
          Cst.Pattern.Alias
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              name_token = token name_syntax_token;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.TYPED_PATTERN -> (
      match direct_non_trivia_nodes node with
      | pattern_node :: type_node :: _ ->
          Cst.Pattern.Typed
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              type_syntax_node = type_node;
            }
      | _ -> Cst.Pattern.Unknown node)
  | Syntax_kind.PAREN_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Parenthesized
            { syntax_node = node; inner = pattern_from_node inner_node }
      | [] -> Cst.Pattern.Unknown node)
  | _ -> Cst.Pattern.Unknown node

and record_pattern_field_from_node node =
  let lifted_field_path =
    let tokens =
      direct_non_trivia_tokens node |> take_tokens_until_equals []
    in
    Cst.ModulePath.{ syntax_node = node; segments = List.map token tokens }
  in
  match Cst.ModulePath.segments lifted_field_path with
  | [] -> None
  | _ ->
      Some
        Cst.RecordPatternField.
          {
            syntax_node = node;
            field_path = lifted_field_path;
            pattern =
              (direct_non_trivia_nodes node
              |> List.find_map (fun child ->
                     match pattern_from_node child with
                     | Cst.Pattern.Unknown _ -> None
                     | pattern -> Some pattern));
          }

let rec apply_argument_from_node node =
  let first_nontrivia_expression_child node =
    direct_non_trivia_nodes node
    |> List.find_map (fun child ->
           match expression_from_node child with
           | Cst.Expression.Unknown _ -> None
           | expr -> Some expr)
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_ARG -> (
      match direct_non_trivia_tokens node with
      | _sigil :: label_syntax_token :: _ ->
          Cst.Labeled
            {
              syntax_node = node;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> Cst.Positional (Cst.Expression.Unknown node))
  | Syntax_kind.OPTIONAL_ARG -> (
      match direct_non_trivia_tokens node with
      | _sigil :: label_syntax_token :: _ ->
          Cst.Optional
            {
              syntax_node = node;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> Cst.Positional (Cst.Expression.Unknown node))
  | _ -> Cst.Positional (expression_from_node node)

and expression_from_node node =
  let known_expression_children node =
    direct_non_trivia_nodes node
    |> List.filter_map (fun child ->
           match expression_from_node child with
           | Cst.Expression.Unknown _ -> None
           | expr -> Some expr)
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.Expression.Path
        { syntax_node = node; path = ident_path_from_node node }
  | Syntax_kind.MODULE_PATH ->
      Cst.Expression.Path
        { syntax_node = node; path = module_path_from_node node }
  | Syntax_kind.ATTRIBUTE_EXPR ->
      Cst.Expression.Attribute (attribute_from_node node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.Expression.Extension (extension_from_node node)
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Expression.Literal (Cst.Literal.Unit { syntax_node = node })
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match field_access_expression_from_node node with
      | Some expr -> expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.STRING_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.ASSIGN_EXPR -> (
      match assign_expression_from_node node with
      | Some expr -> Cst.Expression.Assign expr
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
  | Syntax_kind.FLOAT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Float
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> Cst.Expression.Unknown node)
  | Syntax_kind.CHAR_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Char
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
  | Syntax_kind.ASSERT_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_map (fun child ->
               match expression_from_node child with
               | Cst.Expression.Unknown _ -> None
               | expr -> Some expr)
      with
      | Some asserted ->
          Cst.Expression.Assert { syntax_node = node; asserted }
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.LAZY_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_map (fun child ->
               match expression_from_node child with
               | Cst.Expression.Unknown _ -> None
               | expr -> Some expr)
      with
      | Some body -> Cst.Expression.Lazy { syntax_node = node; body }
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.WHILE_EXPR -> (
      match direct_non_trivia_nodes node with
      | condition_node :: body_node :: _ ->
          Cst.Expression.While
            {
              syntax_node = node;
              condition = expression_from_node condition_node;
              body = expression_from_node body_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.FOR_EXPR -> (
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let direction_token =
        non_trivia_tokens
        |> List.find_opt (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               String.equal text "to" || String.equal text "downto")
      in
      match direct_non_trivia_nodes node, non_trivia_tokens, direction_token with
      | start_node :: end_node :: body_node :: _, _for_kw :: iterator_syntax_token :: _, Some direction_syntax_token ->
          Cst.Expression.For
            {
              syntax_node = node;
              iterator_token = token iterator_syntax_token;
              start_expr = expression_from_node start_node;
              direction_token = token direction_syntax_token;
              end_expr = expression_from_node end_node;
              body = expression_from_node body_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Cst.Expression.Apply
            {
              syntax_node = node;
              callee = expression_from_node callee_node;
              argument = apply_argument_from_node argument_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.POLY_VARIANT_EXPR -> (
      match poly_variant_tag_token node with
      | Some tag_token ->
          Cst.Expression.PolyVariant
            {
              syntax_node = node;
              tag_token;
              payload =
                (direct_non_trivia_nodes node
                |> List.find_map (fun child ->
                       match expression_from_node child with
                       | Cst.Expression.Unknown _ -> None
                       | expr -> Some expr));
            }
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match direct_non_trivia_nodes node with
      | module_syntax_node :: _ ->
          Cst.Expression.FirstClassModule
            {
              syntax_node = node;
              module_syntax_node;
              module_type_syntax_node =
                (direct_non_trivia_nodes node
                |> List.find_opt (fun child ->
                       let kind = Ceibo.Red.SyntaxNode.kind child in
                       kind = Syntax_kind.MODULE_TYPE_PATH
                       || kind = Syntax_kind.MODULE_TYPE_OF));
            }
      | [] -> Cst.Expression.Unknown node)
  | Syntax_kind.LET_MODULE_EXPR -> (
      match let_module_expression_from_node node with
      | Some expr -> Cst.Expression.LetModule expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.LET_EXPR -> (
      match let_expression_parts ~is_recursive_binding:false node with
      | Some (`Exception (exception_decl_node, body_node)) -> (
          match direct_non_trivia_tokens exception_decl_node with
          | _exception_kw :: name_syntax_token :: _ ->
              Cst.Expression.LetException
                {
                  syntax_node = node;
                  exception_declaration =
                    {
                      syntax_node = exception_decl_node;
                      name_token = token name_syntax_token;
                    };
                  body = expression_from_node body_node;
                }
          | _ -> Cst.Expression.Unknown node)
      | _ -> (
          match let_expression_from_node ~is_recursive_binding:false node with
          | Some expr -> Cst.Expression.Let expr
          | None -> Cst.Expression.Unknown node))
  | Syntax_kind.LET_REC_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.Expression.Let expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.TYPED_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: type_node :: _ ->
          Cst.Expression.Typed
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              type_syntax_node = type_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.COERCE_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: to_type_node :: [] ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type_syntax_node = None;
              to_type_syntax_node = to_type_node;
            }
      | expr_node :: from_type_node :: to_type_node :: _ ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type_syntax_node = Some from_type_node;
              to_type_syntax_node = to_type_node;
            }
      | _ -> Cst.Expression.Unknown node)
  | Syntax_kind.PREFIX_EXPR -> (
      match prefix_expression_from_node node with
      | Some expr -> Cst.Expression.Prefix expr
      | None -> Cst.Expression.Unknown node)
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
  | Syntax_kind.SEQUENCE_EXPR -> (
      match sequence_expression_from_node node with
      | Some expr -> Cst.Expression.Sequence expr
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.TUPLE_EXPR ->
      Cst.Expression.Tuple { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.LIST_EXPR ->
      Cst.Expression.List { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.ARRAY_EXPR ->
      Cst.Expression.Array { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.RECORD_EXPR -> (
      match record_literal_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Literal expr)
      | None -> Cst.Expression.Unknown node)
  | Syntax_kind.RECORD_UPDATE_EXPR -> (
      match record_update_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Update expr)
      | None -> Cst.Expression.Unknown node)
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

and index_expression_from_node node =
  match direct_non_trivia_nodes node with
  | collection_node :: index_node :: _ ->
      Some
        Cst.IndexExpression.
          {
            syntax_node = node;
            collection = expression_from_node collection_node;
            index = expression_from_node index_node;
          }
  | _ -> None

and assign_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | target_node :: value_node :: _, operator_syntax_token :: _ ->
      Some
        Cst.AssignExpression.
          {
            syntax_node = node;
            target = expression_from_node target_node;
            operator_token = token operator_syntax_token;
            value = expression_from_node value_node;
          }
  | _ -> None

and prefix_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | operand_node :: _, operator_syntax_token :: _ ->
      Some
        {
          syntax_node = node;
          operator_token = token operator_syntax_token;
          operand = expression_from_node operand_node;
        }
  | _ -> None

and sequence_expression_from_node node =
  match direct_non_trivia_nodes node with
  | left_node :: right_node :: _ ->
      Some
        {
          syntax_node = node;
          left = expression_from_node left_node;
          right = expression_from_node right_node;
        }
  | _ -> None

and record_field_path_from_node node =
  let tokens =
    direct_non_trivia_tokens node |> take_tokens_until_equals []
  in
  Cst.ModulePath.{ syntax_node = node; segments = List.map token tokens }

and record_field_value_from_node node =
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         match expression_from_node child with
         | Cst.Expression.Unknown _ -> None
         | expr -> Some expr)

and record_expression_field_from_node node =
  let lifted_field_path = record_field_path_from_node node in
  match Cst.ModulePath.segments lifted_field_path with
  | [] -> None
  | _ ->
      Some
        Cst.RecordExpressionField.
          {
            syntax_node = node;
            field_path = lifted_field_path;
            value = record_field_value_from_node node;
          }

and record_literal_expression_from_node node =
  let fields =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
    |> List.filter_map record_expression_field_from_node
  in
  Some ({ syntax_node = node; fields } : Cst.record_literal_expression)

and record_update_expression_from_node node =
  match direct_non_trivia_nodes node with
  | base_node :: rest -> (
      let lifted_base =
        match Ceibo.Red.SyntaxNode.kind base_node with
        | Syntax_kind.RECORD_FIELD -> (
            match record_expression_field_from_node base_node with
            | Some { field_path; value = None; _ } ->
                Some
                  (Cst.Expression.Path
                     { syntax_node = base_node; path = field_path })
            | _ -> None)
        | _ -> (
            match expression_from_node base_node with
            | Cst.Expression.Unknown _ -> None
            | expr -> Some expr)
      in
      match lifted_base with
      | Some base ->
          Some
            ( {
                syntax_node = node;
                base;
                fields =
                  rest
                  |> List.filter (fun child ->
                         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
                  |> List.filter_map record_expression_field_from_node;
              }
              : Cst.record_update_expression )
      | None -> None)
  | [] -> None

and local_open_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  let via_let_open =
    non_trivia_tokens
    |> List.exists (fun tok ->
           String.equal (Ceibo.Red.SyntaxToken.text tok) "let")
  in
  let module_path =
    let rec collect_after_open = function
      | [] -> []
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "open" then
            collect_module_tokens [] rest
          else collect_after_open rest
    and collect_module_tokens acc = function
      | [] -> List.rev acc
      | syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "in" then
            List.rev acc
          else if String.equal text "." then
            collect_module_tokens acc rest
          else collect_module_tokens (token syntax_token :: acc) rest
    in
    match collect_after_open non_trivia_tokens with
    | [] -> None
    | lifted_segments ->
        Some Cst.ModulePath.{ syntax_node = node; segments = lifted_segments }
  in
  let prefix_module_path =
    match non_trivia_children with
    | module_path_node :: _body_node :: _ ->
        Some (module_path_like_from_node module_path_node)
    | _ -> None
  in
  let module_path =
    if via_let_open then module_path else prefix_module_path
  in
  let body_expr =
    if via_let_open then
      List.rev non_trivia_children
      |> List.find_map (fun child ->
             match expression_from_node child with
             | Cst.Expression.Unknown _ -> None
             | expr -> Some expr)
    else
      match non_trivia_children with
      | _module_path_node :: body_node :: _ -> (
          match expression_from_node body_node with
          | Cst.Expression.Unknown _ -> None
          | expr -> Some expr)
      | _ -> None
  in
  match module_path, body_expr with
  | Some lifted_module_path, Some lifted_body ->
      Some
        {
          syntax_node = node;
          module_path = lifted_module_path;
          body = lifted_body;
          via_let_open;
        }
  | _ -> None

and let_module_expression_from_node node =
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | _let_kw :: _module_kw :: module_name_token :: _, module_expression_syntax_node :: body_node :: _ ->
      Some
        {
          syntax_node = node;
          module_name_token = token module_name_token;
          module_expression_syntax_node;
          body = expression_from_node body_node;
        }
  | _ -> None

and fun_expression_from_node node =
  match List.rev (direct_non_trivia_nodes node) with
  | body_node :: rev_param_nodes ->
      Some
        Cst.FunExpression.
          {
            syntax_node = node;
            parameters =
              rev_param_nodes
              |> List.rev
              |> List.map parameter_from_node;
            body = expression_from_node body_node;
          }
  | [] -> None

and function_expression_from_node node =
  let match_cases =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
    |> List.filter_map match_case_from_node
  in
  Some Cst.FunctionExpression.{ syntax_node = node; cases = match_cases }

and let_expression_from_node ~is_recursive_binding node =
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          _parameter_nodes,
          bound_value_node,
          and_binding_nodes,
          body_node )) ->
      let lift_and_binding node =
        let direct_children = direct_non_trivia_nodes node in
        let binding_attributes =
          direct_children
          |> List.filter is_attribute_node
          |> List.map attribute_from_node
        in
        let binding_children =
          direct_children |> List.filter (fun child -> not (is_attribute_node child))
        in
        match binding_children with
        | nested_binding_pattern_node :: rest -> (
            match List.rev rest with
            | value_node :: rev_param_nodes ->
                Some
                  Cst.LetBinding.
                    {
                      syntax_node = node;
                      attributes = binding_attributes;
                      binding_pattern = pattern_from_node nested_binding_pattern_node;
                      binding_name =
                        simple_pattern_name_token nested_binding_pattern_node;
                      parameters =
                        rev_param_nodes
                        |> List.rev
                        |> List.filter (fun child ->
                               is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                        |> List.map parameter_from_node;
                      value = expression_from_node value_node;
                      is_recursive = is_recursive_binding;
                    }
            | [] -> None)
        | [] -> None
      in
      Some
        Cst.LetExpression.
          {
            syntax_node = node;
            binding_pattern = pattern_from_node binding_pattern_node;
            bound_value = expression_from_node bound_value_node;
            and_bindings =
              and_binding_nodes
              |> List.filter_map lift_and_binding;
            body = expression_from_node body_node;
            is_recursive = is_recursive_binding;
          }
  | _ -> None

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
  if Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_EXTENSIBLE then
    Cst.TypeDefinition.Extensible { syntax_node = node }
  else
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
        if kind = Syntax_kind.TYPE_EXTENSIBLE then
          Cst.TypeDefinition.Extensible { syntax_node = first }
        else if kind = Syntax_kind.TYPE_CONSTR || kind = Syntax_kind.TYPE_ARROW
           || kind = Syntax_kind.TYPE_TUPLE || kind = Syntax_kind.TYPE_VAR
           || kind = Syntax_kind.TYPE_ALIAS
        then
          Cst.TypeDefinition.Alias { syntax_node = first }
        else if kind = Syntax_kind.FIRST_CLASS_MODULE_TYPE then
          match direct_non_trivia_nodes first with
          | module_type_syntax_node :: _ ->
              Cst.TypeDefinition.FirstClassModule
                { syntax_node = first; module_type_syntax_node }
          | [] -> Cst.TypeDefinition.Other first
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
  let direct_children = direct_non_trivia_nodes node in
  let binding_attributes =
    direct_children
    |> List.filter is_attribute_node
    |> List.map attribute_from_node
  in
  let binding_children =
    direct_children |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_param_nodes ->
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                attributes = binding_attributes;
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters =
                  rev_param_nodes
                  |> List.rev
                  |> List.filter (fun child ->
                         is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                  |> List.map parameter_from_node;
                value = expression_from_node value_node;
                is_recursive = is_recursive_binding;
              }
      | [] -> None)
  | [] -> None

let let_expression_binding_from_node ~is_recursive_binding node =
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          rev_param_nodes,
          bound_value_node,
          _and_binding_nodes,
          _body_node )) ->
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                attributes = [];
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters =
                  rev_param_nodes
                  |> List.filter (fun child ->
                         is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                  |> List.map parameter_from_node;
                value = expression_from_node bound_value_node;
                is_recursive = is_recursive_binding;
              }
  | _ -> None

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

let declaration_name_token_from_node node =
  let operator_token_from_node node =
    direct_non_trivia_tokens node
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not (String.equal text "(" || String.equal text ")"))
    |> Option.map token
  in
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.IDENT_EXPR ->
             direct_non_trivia_tokens child |> List.find_opt (fun _ -> true) |> Option.map token
         | Syntax_kind.OPERATOR_PATTERN -> operator_token_from_node child
         | _ -> None)

let value_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  match declaration_name_token_from_node node with
  | Some lifted_name_token -> (
      match List.rev direct_children with
      | lifted_type_syntax_node :: _ ->
          Some
            ({ syntax_node = node; name_token = lifted_name_token; type_syntax_node = lifted_type_syntax_node }
              : Cst.value_declaration)
      | [] -> None)
  | None -> None

let external_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let lifted_primitive_name_tokens =
    direct_non_trivia_tokens node
    |> List.filter (fun syntax_token ->
           Ceibo.Red.SyntaxToken.kind syntax_token = Syntax_kind.STRING_LITERAL)
    |> List.map token
  in
  let external_name_token =
    match direct_non_trivia_tokens node with
    | _external_kw :: name_syntax_token :: _ -> Some (token name_syntax_token)
    | _ -> None
  in
  match external_name_token with
  | Some lifted_name_token -> (
      match direct_children with
      | lifted_type_syntax_node :: _ ->
          Some
            ({
               syntax_node = node;
               name_token = lifted_name_token;
               type_syntax_node = lifted_type_syntax_node;
               primitive_name_tokens = lifted_primitive_name_tokens;
             }
              : Cst.external_declaration)
      | _ -> None)
  | None -> None

let include_statement_from_node node =
  match direct_non_trivia_nodes node with
  | lifted_included_syntax_node :: _ ->
      Some
        ({ syntax_node = node; included_syntax_node = lifted_included_syntax_node }
          : Cst.include_statement)
  | [] -> None

let exception_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _exception_kw :: name_syntax_token :: _ ->
      Some
        ({ syntax_node = node; name_token = token name_syntax_token }
          : Cst.exception_declaration)
  | _ -> None

let rec collect_let_bindings node =
  let bindings_here =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.LET_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:false node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_REC_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:true node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:false node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_REC_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:true node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
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
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ Cst.Item.ValueDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.Item.ExternalDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.Item.IncludeStatement stmt ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.Item.ExceptionDeclaration decl ]
      | None -> [ Cst.Item.Unknown node ])
  | Syntax_kind.SEQUENCE_EXPR -> (
      match direct_non_trivia_nodes node with
      | only_expr :: [] -> (
          match expression_from_node only_expr with
          | Cst.Expression.Unknown _ -> [ Cst.Item.Unknown node ]
          | expr -> [ Cst.Item.Expression expr ])
      | _ -> (
          match expression_from_node node with
          | Cst.Expression.Unknown _ -> [ Cst.Item.Unknown node ]
          | expr -> [ Cst.Item.Expression expr ]))
  | _ -> (
      match expression_from_node node with
      | Cst.Expression.Unknown _ -> [ Cst.Item.Unknown node ]
      | expr -> [ Cst.Item.Expression expr ])

let build_source_file_body tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  let file_let_bindings = collect_let_bindings root in
  let file_expressions = collect_expressions root in
  (root, file_items, file_let_bindings, file_expressions)

let rec validate_pattern ~context = function
  | Cst.Pattern.Identifier _ | Cst.Pattern.Wildcard _ | Cst.Pattern.Literal _ -> ()
  | Cst.Pattern.Lazy { pattern; _ } ->
      validate_pattern ~context:("pattern.lazy" :: context) pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      validate_pattern ~context:("pattern.exception" :: context) pattern
  | Cst.Pattern.Range _ ->
      ()
  | Cst.Pattern.FirstClassModule _ ->
      ()
  | Cst.Pattern.PolyVariant { payload; _ } ->
      Option.iter
        (validate_pattern ~context:("pattern.poly_variant.payload" :: context))
        payload
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.iteri
        (fun index argument ->
          validate_pattern
            ~context:
              (("pattern.constructor.argument[" ^ Int.to_string index ^ "]")
             :: context)
            argument)
        arguments
  | Cst.Pattern.Tuple { elements; _ }
  | Cst.Pattern.List { elements; _ }
  | Cst.Pattern.Array { elements; _ }
  | Cst.Pattern.Or { alternatives = elements; _ } ->
      List.iteri
        (fun index pattern ->
          validate_pattern
            ~context:(("pattern.element[" ^ Int.to_string index ^ "]") :: context)
            pattern)
        elements
  | Cst.Pattern.Record { fields; _ } ->
      List.iteri
        (fun index field ->
          Option.iter
            (validate_pattern
               ~context:
                 (("pattern.record.field[" ^ Int.to_string index ^ "].pattern")
                 :: context))
            (Cst.RecordPatternField.pattern field))
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      validate_pattern ~context:("pattern.cons.head" :: context) head;
      validate_pattern ~context:("pattern.cons.tail" :: context) tail
  | Cst.Pattern.Alias { pattern; _ } ->
      validate_pattern ~context:("pattern.alias.pattern" :: context) pattern
  | Cst.Pattern.Typed { pattern; _ } ->
      validate_pattern ~context:("pattern.typed.pattern" :: context) pattern
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

and validate_apply_argument ~context = function
  | Cst.Positional expr ->
      validate_expression ~context:("apply_argument.positional" :: context) expr
  | Cst.Labeled { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.labeled.value" :: context))
        value
  | Cst.Optional { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.optional.value" :: context))
        value

and validate_expression ~context = function
  | Cst.Expression.Path _ | Cst.Expression.Literal _
  | Cst.Expression.Attribute _ | Cst.Expression.Extension _
  | Cst.Expression.FirstClassModule _ ->
      ()
  | Cst.Expression.LetModule { body; _ } ->
      validate_expression ~context:("expression.let_module.body" :: context) body
  | Cst.Expression.LetException { body; _ } ->
      validate_expression ~context:("expression.let_exception.body" :: context)
        body
  | Cst.Expression.PolyVariant { payload; _ } ->
      Option.iter
        (validate_expression ~context:("expression.poly_variant.payload" :: context))
        payload
  | Cst.Expression.Assert { asserted; _ } ->
      validate_expression ~context:("expression.assert.asserted" :: context)
        asserted
  | Cst.Expression.Lazy { body; _ } ->
      validate_expression ~context:("expression.lazy.body" :: context) body
  | Cst.Expression.While { condition; body; _ } ->
      validate_expression ~context:("expression.while.condition" :: context)
        condition;
      validate_expression ~context:("expression.while.body" :: context) body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      validate_expression ~context:("expression.for.start" :: context) start_expr;
      validate_expression ~context:("expression.for.end" :: context) end_expr;
      validate_expression ~context:("expression.for.body" :: context) body
  | Cst.Expression.Apply { callee; argument; _ } ->
      validate_expression ~context:("expression.apply.callee" :: context) callee;
      validate_apply_argument ~context:("expression.apply.argument" :: context)
        argument
  | Cst.Expression.Prefix { operand; _ } ->
      validate_expression ~context:("expression.prefix.operand" :: context)
        operand
  | Cst.Expression.FieldAccess { receiver; _ } ->
      validate_expression ~context:("expression.field_access.receiver" :: context)
        receiver
  | Cst.Expression.Index { collection; index; _ } ->
      validate_expression ~context:("expression.index.collection" :: context)
        collection;
      validate_expression ~context:("expression.index.index" :: context) index
  | Cst.Expression.Assign { target; value; _ } ->
      validate_expression ~context:("expression.assign.target" :: context) target;
      validate_expression ~context:("expression.assign.value" :: context) value
  | Cst.Expression.Infix { left; right; _ } ->
      validate_expression ~context:("expression.infix.left" :: context) left;
      validate_expression ~context:("expression.infix.right" :: context) right
  | Cst.Expression.Typed { expression; _ } ->
      validate_expression ~context:("expression.typed.expression" :: context)
        expression
  | Cst.Expression.Coerce { expression; _ } ->
      validate_expression ~context:("expression.coerce.expression" :: context)
        expression
  | Cst.Expression.Sequence { left; right; _ } ->
      validate_expression ~context:("expression.sequence.left" :: context) left;
      validate_expression ~context:("expression.sequence.right" :: context) right
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      List.iteri
        (fun index expr ->
          validate_expression
            ~context:(("expression.element[" ^ Int.to_string index ^ "]") :: context)
            expr)
        elements
  | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
      List.iteri
        (fun index field ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.record.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            (Cst.RecordExpressionField.value field))
        fields
  | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
      validate_expression ~context:("expression.record.base" :: context) base;
      List.iteri
        (fun index field ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.record.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            (Cst.RecordExpressionField.value field))
        fields
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
  | Cst.Expression.Let { binding_pattern; bound_value; and_bindings; body; _ } ->
      validate_pattern ~context:("expression.let.pattern" :: context) binding_pattern;
      validate_expression ~context:("expression.let.bound_value" :: context) bound_value;
      List.iteri
        (fun index binding ->
          validate_pattern
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].pattern")
              :: context)
            (Cst.LetBinding.binding_pattern binding);
          validate_expression
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].value")
              :: context)
            (Cst.LetBinding.value binding))
        and_bindings;
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
  | Cst.TypeDefinition.Extensible _ -> ()
  | Cst.TypeDefinition.FirstClassModule _ -> ()
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
  | Cst.Item.LetBinding { binding_pattern; value; _ } ->
      validate_pattern ~context:("item.let_binding.pattern" :: context)
        binding_pattern;
      validate_expression ~context:("item.let_binding.value" :: context) value
  | Cst.Item.Expression expr ->
      validate_expression ~context:("item.expression" :: context) expr
  | Cst.Item.ModuleDeclaration _ | Cst.Item.ModuleTypeDeclaration _
  | Cst.Item.OpenStatement _ | Cst.Item.ValueDeclaration _
  | Cst.Item.ExternalDeclaration _ | Cst.Item.IncludeStatement _
  | Cst.Item.ExceptionDeclaration _ ->
      ()
  | Cst.Item.Unknown syntax_node ->
      bail ~message:"unsupported structure item during Ceibo -> CST lifting"
        ~syntax_node ~context

let validate_source_file source_file =
  List.iteri
    (fun index item ->
      validate_item ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ] item)
    (Cst.SourceFile.items source_file);
  List.iteri
    (fun index binding ->
      validate_expression
        ~context:[ "source_file.let_bindings[" ^ Int.to_string index ^ "].value" ]
        (Cst.LetBinding.value binding))
    (Cst.SourceFile.let_bindings source_file);
  List.iteri
    (fun index expr ->
      validate_expression
        ~context:[ "source_file.expressions[" ^ Int.to_string index ^ "]" ]
        expr)
    (Cst.SourceFile.expressions source_file)

let lift ~kind tree =
  let syntax_node, items, let_bindings, expressions = build_source_file_body tree in
  let cst =
    match kind with
    | `Implementation ->
        Cst.Implementation { syntax_node; items; let_bindings; expressions }
    | `Interface ->
        Cst.Interface { syntax_node; items; let_bindings; expressions }
  in
  validate_source_file cst;
  cst

let create_from_ceibo ~kind tree =
  match lift ~kind tree with
  | cst -> Ok cst
  | exception Bail error -> Error error
