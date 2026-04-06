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

let source_slice = fun ~source span ->
  let len = Syn.Ceibo.Span.(span.end_ - span.start) in
  String.sub source span.start len

let expression_source = fun ~source expr ->
  source_slice ~source (Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
  |> String.trim

let make_fix = fun ~source (expr: Syn.Cst.if_expression) ->
  let condition = expression_source ~source expr.condition in
  let then_branch = expression_source ~source expr.then_branch in
  Fix.make
    ~title:"Remove redundant else () branch"
    ~operations:[
      Fix.replace_node_with_text
        ~target:expr.syntax_node
        ~text:(("if " ^ condition ^ " then " ^ then_branch));
    ]

let make_diagnostic = fun ~source (expr: Syn.Cst.if_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Remove else () from this if expression."
    ~fix:(make_fix ~source expr)
    ()

let diagnostic_for_expression = fun ~source ->
  function
  | Syn.Cst.Expression.If expr -> (
      match expr.else_branch with
      | Some else_branch when is_unit_expression else_branch -> Some (make_diagnostic ~source expr)
      | _ -> None
    )
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.expressions_of_structure_item
  |> List.filter_map (diagnostic_for_expression ~source:ctx.source)

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
