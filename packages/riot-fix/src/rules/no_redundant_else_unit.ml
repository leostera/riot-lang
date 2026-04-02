open Std

let rule_id = "no-redundant-else-unit"

let rule_description = "Else branches that only return unit should be omitted"

let rule_explain = {|
When the only purpose of the conditional is to run an effect conditionally, `if cond
then expr` already says that. Adding `else ()` repeats the same idea in a noisier
form.

Omitting the trivial unit branch makes the control flow easier to scan, especially in
effect-heavy code where these small conditionals show up often.

Keep the `else` branch when it has real behavior. Drop it when it is only there to say
"otherwise, do nothing."
|}

let rec is_unit_expression = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _) -> true
  | Syn.Cst.Expression.Parenthesized expr -> is_unit_expression expr.inner
  | _ -> false

let make_diagnostic = fun (expr: Syn.Cst.if_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Remove else () from this if expression."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.If expr -> (
      match expr.else_branch with
      | Some else_branch when is_unit_expression else_branch -> Some (make_diagnostic expr)
      | _ -> None
    )
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.expressions_of_structure_item
  |> List.filter_map diagnostic_for_expression

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
