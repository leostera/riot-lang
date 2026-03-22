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

let rec child_expressions = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unknown _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      [ Syn.Cst.ApplyExpression.callee expr; Syn.Cst.ApplyExpression.argument expr ]
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      [ receiver ]
  | Syn.Cst.Expression.Infix expr ->
      [ Syn.Cst.InfixExpression.left expr; Syn.Cst.InfixExpression.right expr ]
  | Syn.Cst.Expression.Fun expr ->
      [ Syn.Cst.FunExpression.body expr ]
  | Syn.Cst.Expression.Function expr ->
      Syn.Cst.FunctionExpression.cases expr
      |> List.concat_map (fun case ->
             (match Syn.Cst.MatchCase.guard case with
             | Some guard -> [ guard ]
             | None -> [])
             @ [ Syn.Cst.MatchCase.body case ])
  | Syn.Cst.Expression.Let expr ->
      [ Syn.Cst.LetExpression.bound_value expr; Syn.Cst.LetExpression.body expr ]
  | Syn.Cst.Expression.Match expr ->
      Syn.Cst.MatchExpression.scrutinee expr
      :: (Syn.Cst.MatchExpression.cases expr
         |> List.concat_map (fun case ->
                (match Syn.Cst.MatchCase.guard case with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ Syn.Cst.MatchCase.body case ]))
  | Syn.Cst.Expression.Try expr ->
      Syn.Cst.TryExpression.body expr
      :: (Syn.Cst.TryExpression.cases expr
         |> List.concat_map (fun case ->
                (match Syn.Cst.MatchCase.guard case with
                | Some guard -> [ guard ]
                | None -> [])
                @ [ Syn.Cst.MatchCase.body case ]))
  | Syn.Cst.Expression.If expr ->
      let base =
        [ Syn.Cst.IfExpression.condition expr; Syn.Cst.IfExpression.then_branch expr ]
      in
      (match Syn.Cst.IfExpression.else_branch expr with
      | Some else_branch -> base @ [ else_branch ]
      | None -> base)
  | Syn.Cst.Expression.Parenthesized expr ->
      [ Syn.Cst.ParenthesizedExpression.inner expr ]

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

let make_diagnostic expr depth =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Cst.MatchExpression.syntax_node expr |> Syn.Ceibo.Red.SyntaxNode.span)
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
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.concat_map (fun binding ->
             diagnostics_for_expression ~inside_match:false
               (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
