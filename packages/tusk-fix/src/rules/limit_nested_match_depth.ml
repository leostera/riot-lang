open Std

let rule_id = "limit-nested-match-depth"
let rule_name = "Limit Nested Match Depth"
let rule_code = "F0135"

let rule_description =
  "Deep towers of nested match expressions should be flattened"

let rule_message =
  "Deep towers of nested match expressions should be flattened."

let rule_explain =
  {|
Avoid nesting `match` expressions three levels deep or more.

Deep match towers make control flow harder to scan because each branch introduces another layer of indentation and another set of cases to keep in your head.
Once the nesting gets this deep, the code usually wants helper functions, smaller pattern matches, or a different data shape.

Examples:
  Avoid:
    match x with
    | _ ->
        match y with
        | _ ->
            match z with
            | _ -> ...

  Better:
    let render_z z = ...
    in
    match x with
    | _ ->
        match y with
        | _ -> render_z z
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
  | Syn.Cst.Expression.Function expr ->
      child_expressions_of_function_body expr.body
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
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
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
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.concat_map (fun binding ->
             diagnostics_for_expression ~inside_match:false
               (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
