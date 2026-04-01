open Std

let rule_id = "limit-parenthesis-depth"

let rule_description = "Deep chains of parenthesized expressions should be avoided"

let rule_explain = {|
Heavy parenthesization is usually a sign that the code wants to be flatter.
When readers have to count closing delimiters to understand an expression, the shape
of the program is doing more work than the names inside it.

Sometimes the right fix is simply to remove redundant grouping. Other times the
expression wants an intermediate name, a helper function, or a pipeline that makes
evaluation order obvious without so much punctuation.

If a line keeps growing more parentheses just to stay understandable, it is already
telling you that the current shape is too dense.
|}

let max_parenthesis_depth = 5

let make_diagnostic = fun (expr: Syn.Cst.parenthesized_expression) depth ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:((expr.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span))
    ~suggestion:(("Reduce parenthesis depth from " ^ Int.to_string depth ^ " by removing redundant grouping or extracting a named value"))
    ()

let rec parenthesis_chain_depth = function
  | Syn.Cst.Expression.Parenthesized expr -> 1 + parenthesis_chain_depth expr.inner
  | _ -> 0

let rec diagnostics_for_function_body = fun ~inside_parenthesized_chain ->
  function
  | Syn.Cst.Expression expression -> diagnostics_for_expression ~inside_parenthesized_chain expression
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.concat_map
        (fun (case: Syn.Cst.match_case) ->
          (
            match case.guard with
            | Some guard -> diagnostics_for_expression ~inside_parenthesized_chain guard
            | None -> []
          ) @ diagnostics_for_expression ~inside_parenthesized_chain case.body)

and diagnostics_for_expression = fun ~inside_parenthesized_chain ->
  function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      diagnostics_for_expression ~inside_parenthesized_chain expr.callee @ (
        match expr.argument with
        | Syn.Cst.Positional argument -> diagnostics_for_expression ~inside_parenthesized_chain argument
        | Syn.Cst.Labeled { value; _ }
        | Syn.Cst.Optional { value; _ } -> (
            match value with
            | Some value -> diagnostics_for_expression ~inside_parenthesized_chain value
            | None -> []
          )
      )
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      diagnostics_for_expression ~inside_parenthesized_chain receiver
  | Syn.Cst.Expression.Infix expr ->
      diagnostics_for_expression ~inside_parenthesized_chain (Syn.Cst.InfixExpression.left expr)
      @ diagnostics_for_expression ~inside_parenthesized_chain (Syn.Cst.InfixExpression.right expr)
  | Syn.Cst.Expression.Fun expr ->
      diagnostics_for_function_body ~inside_parenthesized_chain expr.body
  | Syn.Cst.Expression.Function { syntax_node; cases; _ } ->
      diagnostics_for_function_body
        ~inside_parenthesized_chain
        (Syn.Cst.Cases { syntax_node; cases })
  | Syn.Cst.Expression.Let expr ->
      diagnostics_for_expression ~inside_parenthesized_chain expr.bound_value
      @ diagnostics_for_expression ~inside_parenthesized_chain expr.body
  | Syn.Cst.Expression.Match expr ->
      diagnostics_for_expression ~inside_parenthesized_chain expr.scrutinee @ (
        expr.cases |> List.concat_map
          (fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> diagnostics_for_expression ~inside_parenthesized_chain guard
              | None -> []
            ) @ diagnostics_for_expression ~inside_parenthesized_chain case.body)
      )
  | Syn.Cst.Expression.Try expr ->
      diagnostics_for_expression ~inside_parenthesized_chain expr.body @ (
        expr.cases |> List.concat_map
          (fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> diagnostics_for_expression ~inside_parenthesized_chain guard
              | None -> []
            ) @ diagnostics_for_expression ~inside_parenthesized_chain case.body)
      )
  | Syn.Cst.Expression.If expr ->
      diagnostics_for_expression ~inside_parenthesized_chain expr.condition
      @ diagnostics_for_expression ~inside_parenthesized_chain expr.then_branch
      @ (
        match expr.else_branch with
        | Some else_branch -> diagnostics_for_expression ~inside_parenthesized_chain else_branch
        | None -> []
      )
  | Syn.Cst.Expression.Parenthesized expr ->
      let inner = expr.inner in
      let nested = diagnostics_for_expression ~inside_parenthesized_chain:true inner in
      if inside_parenthesized_chain then
        nested
      else
        let depth = parenthesis_chain_depth (Syn.Cst.Expression.Parenthesized expr) in
        if depth >= max_parenthesis_depth then
          make_diagnostic expr depth :: nested
        else
          nested
  | _ ->
      []

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.concat_map
    (fun binding ->
      diagnostics_for_expression ~inside_parenthesized_chain:false (Syn.Cst.LetBinding.value binding))

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
