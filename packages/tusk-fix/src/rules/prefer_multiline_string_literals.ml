open Std

let rule_id = "prefer-multiline-string-literals"
let rule_description =
  "String literal chains should use multiline string literals instead of repeated concatenation"

let rule_explain =
  {explain|
When a string literal starts spilling across several concatenated pieces, the reader
has to reconstruct the final text mentally from punctuation. That is tedious for short
examples and error-prone for longer payloads, especially when whitespace matters.

A multiline string literal keeps the content in one place and lets the eye inspect the
actual output instead of a chain of `^` operators. This is particularly helpful for
HTML fragments, SQL, JSON examples, and test fixtures where punctuation and spacing
carry meaning.

Use concatenation when the pieces are genuinely dynamic. Use a multiline literal when
the content is really one static block of text.
|explain}

let rec string_literal_chain_size_in_function_body = function
  | Syn.Cst.Expression expression ->
      string_literal_chain_size expression
  | Syn.Cst.Cases { cases; _ } ->
      cases
      |> List.find_map (fun (case : Syn.Cst.match_case) ->
             match case.guard with
             | Some guard -> (
                 match string_literal_chain_size guard with
                 | Some _ as size -> size
                 | None -> string_literal_chain_size case.body)
             | None -> string_literal_chain_size case.body)

and string_literal_chain_size = function
  | Syn.Cst.Expression.Path _ -> None
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) -> Some 1
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Apply _ ->
      None
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      string_literal_chain_size receiver
  | Syn.Cst.Expression.Fun expr ->
      string_literal_chain_size_in_function_body expr.body
  | Syn.Cst.Expression.Function { syntax_node; cases; _ } ->
      string_literal_chain_size_in_function_body (Syn.Cst.Cases { syntax_node; cases })
  | Syn.Cst.Expression.Parenthesized expr ->
      string_literal_chain_size expr.inner
  | Syn.Cst.Expression.Infix expr
    when String.equal (Syn.Cst.InfixExpression.operator expr) "^" -> (
      match
        string_literal_chain_size (Syn.Cst.InfixExpression.left expr),
        string_literal_chain_size (Syn.Cst.InfixExpression.right expr)
      with
      | Some left_count, Some right_count -> Some (left_count + right_count)
      | _ -> None)
  | Syn.Cst.Expression.Let expr -> (
      match string_literal_chain_size expr.bound_value with
      | Some _ as size -> size
      | None -> string_literal_chain_size expr.body)
  | Syn.Cst.Expression.Match expr -> (
      match string_literal_chain_size expr.scrutinee with
      | Some _ as size -> size
      | None ->
          expr.cases
          |> List.find_map (fun (case : Syn.Cst.match_case) ->
                 match case.guard with
                 | Some guard -> (
                     match string_literal_chain_size guard with
                     | Some _ as size -> size
                     | None -> string_literal_chain_size case.body)
                 | None -> string_literal_chain_size case.body))
  | Syn.Cst.Expression.Try expr -> (
      match string_literal_chain_size expr.body with
      | Some _ as size -> size
      | None ->
          expr.cases
          |> List.find_map (fun (case : Syn.Cst.match_case) ->
                 match case.guard with
                 | Some guard -> (
                     match string_literal_chain_size guard with
                     | Some _ as size -> size
                     | None -> string_literal_chain_size case.body)
                 | None -> string_literal_chain_size case.body))
  | Syn.Cst.Expression.If expr -> (
      match string_literal_chain_size expr.then_branch with
      | Some _ as size -> size
      | None -> (
          match expr.else_branch with
          | Some else_branch -> string_literal_chain_size else_branch
          | None -> None))
  | Syn.Cst.Expression.Infix _
  ->
      None
  | _ ->
      None

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:"Use a multiline string literal like {| ... |} instead of concatenating string literals"
    ()

let diagnostic_for_binding binding =
  match string_literal_chain_size (Syn.Cst.LetBinding.value binding) with
  | Some count when count >= 2 ->
      Some (make_diagnostic (Syn.Cst.LetBinding.value binding))
  | Some _ | None ->
      None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
