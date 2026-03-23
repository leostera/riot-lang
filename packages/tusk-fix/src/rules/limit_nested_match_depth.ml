open Std

let rule_id = "limit-nested-match-depth"
let rule_description =
  "Deep towers of nested match expressions should be flattened"

let rule_explain =
  {|
Three layers of nested `match` usually means the control flow has stopped fitting in
the reader's head. Each additional `match` introduces another indentation level,
another set of cases, and another question about which values are still in scope.

When a branch has to open yet another `match`, the code often wants one of three
things instead: a helper function, a smaller intermediate value, or a different data
shape that captures the combination up front.

As a rough rule, once you are writing `match x with ... match y with ... match z with`,
stop and ask whether the innermost logic deserves a name of its own.
|}

let max_nested_match_depth = 3

let rec child_expressions_of_function_body = function
  | Syn.Cst.Expression expression ->
      [ expression ]
  | Syn.Cst.Cases { cases; _ } ->
      cases
      |> List.concat_map (fun (case : Syn.Cst.match_case) ->
             (match case.guard with
             | Some guard -> [ guard ]
             | None -> [])
             @ [ case.body ])

and child_expressions = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      expr.callee
      ::
      (match expr.argument with
      | Syn.Cst.Positional argument -> [ argument ]
      | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
          Option.to_list value)
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      [ receiver ]
  | Syn.Cst.Expression.Infix expr ->
      [ Syn.Cst.InfixExpression.left expr; Syn.Cst.InfixExpression.right expr ]
  | Syn.Cst.Expression.Fun expr ->
      child_expressions_of_function_body expr.body
  | Syn.Cst.Expression.Function { syntax_node; cases; _ } ->
      child_expressions_of_function_body (Syn.Cst.Cases { syntax_node; cases })
  | Syn.Cst.Expression.Let expr ->
      [ expr.bound_value; expr.body ]
  | Syn.Cst.Expression.Match expr ->
      expr.scrutinee
      :: (expr.cases
         |> List.concat_map (fun (case : Syn.Cst.match_case) ->
                (match case.guard with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ case.body ]))
  | Syn.Cst.Expression.Try expr ->
      expr.body
      :: (expr.cases
         |> List.concat_map (fun (case : Syn.Cst.match_case) ->
                (match case.guard with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ case.body ]))
  | Syn.Cst.Expression.If expr ->
      let base = [ expr.condition; expr.then_branch ] in
      (match expr.else_branch with
      | Some else_branch -> base @ [ else_branch ]
      | None -> base)
  | Syn.Cst.Expression.Parenthesized expr ->
      [ expr.inner ]
  | _ ->
      []

let max_list = function
  | [] -> 0
  | xs -> List.fold_left max 0 xs

let rec match_chain_depth = function
  | Syn.Cst.Expression.Match expr ->
      1
      + (child_expressions (Syn.Cst.Expression.Match expr)
        |> List.map match_chain_depth
        |> max_list)
  | expr -> child_expressions expr |> List.map match_chain_depth |> max_list

let make_diagnostic (expr : Syn.Cst.match_expression) depth =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(expr.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:
      ("Reduce nested match depth from " ^ Int.to_string depth
     ^ " by extracting a helper or flattening the control flow.")
    ()

let rec diagnostics_for_expression ~inside_match = function
  | Syn.Cst.Expression.Match expr ->
      let nested =
        child_expressions (Syn.Cst.Expression.Match expr)
        |> List.concat_map (diagnostics_for_expression ~inside_match:true)
      in
      if inside_match then
        nested
      else
        let depth = match_chain_depth (Syn.Cst.Expression.Match expr) in
        if depth >= max_nested_match_depth then
          make_diagnostic expr depth :: nested
        else
          nested
  | expr ->
      child_expressions expr
      |> List.concat_map (diagnostics_for_expression ~inside_match)

let check_tree (ctx : Rule.context) _red_root =
  let source_file = ctx.cst in
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.concat_map (fun binding ->
             diagnostics_for_expression ~inside_match:false
               (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
