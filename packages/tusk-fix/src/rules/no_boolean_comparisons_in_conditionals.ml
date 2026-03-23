open Std

let rule_id = "no-boolean-comparisons-in-conditionals"
let rule_description =
  "Boolean conditions should not be compared explicitly to true or false"

let rule_explain =
  {|
When the condition already has type `bool`, comparing it to `true` or `false` adds
noise without adding information. `if is_ready then ...` says exactly what
`if is_ready = true then ...` says, but with less punctuation between the reader and
the real condition.

The same applies on the negative side. `if not is_ready then ...` is easier to read
than `if is_ready = false then ...` because the negation is explicit instead of
encoded as an equality test.

This rule exists to keep conditionals focused on the condition itself rather than on
an unnecessary comparison wrapped around it.
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_parens expr.inner
  | expr -> expr

let bool_literal_value = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Bool { literal_token; _ }) ->
      Some (String.equal (Syn.Cst.Token.text literal_token) "true")
  | _ -> None

let comparison_operands expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Infix (cmp : Syn.Cst.infix_expression) ->
      let op = Syn.Cst.InfixExpression.operator cmp in
      if String.equal op "=" || String.equal op "!=" || String.equal op "<>" then
        Some (op, Syn.Cst.InfixExpression.left cmp, Syn.Cst.InfixExpression.right cmp)
      else
        None
  | _ -> None

let should_flag_condition expr =
  match comparison_operands expr with
  | Some (_, left, right) -> (
      match bool_literal_value left, bool_literal_value right with
      | Some _, None | None, Some _ -> true
      | _ -> false)
  | None -> false

let suggestion_for_condition expr =
  match comparison_operands expr with
  | Some (op, left, right) -> (
      match bool_literal_value left, bool_literal_value right with
      | Some true, None
      | None, Some true ->
          if String.equal op "=" then
            "Use the condition directly."
          else
            "Use not <condition> instead."
      | Some false, None
      | None, Some false ->
          if String.equal op "=" then
            "Use not <condition> instead."
          else
            "Use the condition directly."
      | _ -> "Simplify this boolean comparison.")
  | None -> "Simplify this boolean comparison."

let make_diagnostic (if_expr : Syn.Cst.if_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span if_expr.syntax_node)
    ~suggestion:(suggestion_for_condition if_expr.condition)
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.If if_expr
    when should_flag_condition if_expr.condition ->
      Some (make_diagnostic if_expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  let source_file = ctx.cst in
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.expressions_of_structure_item
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
