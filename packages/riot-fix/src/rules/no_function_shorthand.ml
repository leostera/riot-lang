open Std

let rule_id = "no-function-shorthand"

let rule_description = "Named functions should avoid `function` shorthand and use explicit parameters instead"

let rule_explain = {|
`function` is compact, but it hides the parameter list right where a named function is
introducing its public shape. With `let describe = function ...`, the reader has to
enter the branches before they even know what the argument is called.

For local throwaway lambdas that tradeoff can be fine. For named functions, explicit
parameters age better. `let describe value = match value with ...` gives the argument
a name immediately and makes later refactors, logging, and type annotations simpler.

This rule nudges named functions toward the clearer form while still leaving `function`
available for places where the shorthand is genuinely a better fit.
|}

let make_fix = fun (expr: Syn.Cst.function_expression) ->
  Fix.make
    ~title:"Rewrite function shorthand as an explicit parameter match"
    ~operations:
      [
        Fix.replace_token_with_text
          ~target:(Syn.Cst.Token.syntax_token expr.keyword_token)
          ~text:"fun value -> match value with";
      ]

let make_diagnostic = fun binding ->
  let value = Syn.Cst.LetBinding.value binding in
  let fix =
    match value with
    | Syn.Cst.Expression.Function expr -> Some (make_fix expr)
    | _ -> None
  in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.LetBinding.value_syntax_node binding))
    ~suggestion:"Use explicit parameters with `let name x = ...` or `let name = fun x -> ...`"
    ?fix
    ()

let diagnostic_for_binding = fun binding ->
  if
    Syn.Ceibo.Red.SyntaxNode.kind (Syn.Cst.LetBinding.value_syntax_node binding) = Syn.SyntaxKind.FUNCTION_EXPR
  then
    Some (make_diagnostic binding)
  else
    None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.filter Syn.Cst.LetBinding.is_function
  |> List.filter_map diagnostic_for_binding

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
