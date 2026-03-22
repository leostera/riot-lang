open Std

let rule_id = "prefer-pipelines-for-nested-calls"
let rule_name = "Prefer Pipelines For Nested Calls"
let rule_code = "F0129"

let rule_description =
  "Deeply nested function calls should usually be written as pipelines"

let rule_message =
  "Prefer pipelines over deeply nested function calls."

let rule_explain =
  {|
Deeply nested function calls are easier to read when written as pipelines.

Why this rule exists:
- `foo (bar (baz (hex 1)))` forces the reader to scan inside-out.
- Pipelines read in execution order and make each transformation step explicit.
- Once a call chain gets deep enough, `|>` is usually easier on the eyes.

Examples:
  Bad:    foo (bar (baz (hex 1)))
  Better: hex 1 |> baz |> bar |> foo
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_parens (Syn.Cst.ParenthesizedExpression.inner expr)
  | expr -> expr

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } -> value

let rec nested_call_count expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply apply ->
      (match expression_of_apply_argument (Syn.Cst.ApplyExpression.argument apply) with
      | Some argument -> 1 + nested_call_count argument
      | None -> 1)
  | _ -> 0

let threshold = 4

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:"Rewrite this call chain as a pipeline."
    ()

let diagnostic_for_expression expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply _ when nested_call_count expr >= threshold ->
      Some (make_diagnostic expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
