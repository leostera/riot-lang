open Std

let rule_id = "no-redundant-parentheses"

let rule_description = "Obvious grouping parentheses should be removed"

let rule_explain = {|
Parentheses are valuable when they explain precedence or mark a meaningful grouping.
When they only wrap a single identifier, literal, or another parenthesized expression,
they stop clarifying anything and start adding visual noise.

This rule only targets the cases where the grouping is already obvious. It is not a
general attack on parentheses. Keep them when they disambiguate an expression, but do
not leave them around as punctuation residue once they stop doing real work.
|}

let rec child_expressions_of_function_body = function
  | Syn.Cst.Expression expression -> [ expression ]
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.map
        ~fn:(fun (case: Syn.Cst.match_case) ->
          (
            match case.guard with
            | Some guard -> [ guard ]
            | None -> []
          ) @ [ case.body ]) |> List.concat

and child_expressions = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      expr.callee :: (
        match expr.argument with
        | Syn.Cst.Positional argument -> [ argument ]
        | Syn.Cst.Labeled { value; _ }
        | Syn.Cst.Optional { value; _ } -> Option.to_list value
      )
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
      expr.scrutinee :: (
        expr.cases |> List.map
          ~fn:(fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> [ guard ]
              | None -> []
            ) @ [ case.body ]) |> List.concat
      )
  | Syn.Cst.Expression.Try expr ->
      expr.body :: (
        expr.cases |> List.map
          ~fn:(fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> [ guard ]
              | None -> []
            ) @ [ case.body ]) |> List.concat
      )
  | Syn.Cst.Expression.If expr ->
      let base = [ expr.condition; expr.then_branch ] in
      (
        match expr.else_branch with
        | Some else_branch -> base @ [ else_branch ]
        | None -> base
      )
  | Syn.Cst.Expression.Parenthesized expr ->
      [ expr.inner ]
  | _ ->
      []

let opens_with_begin = fun ({ syntax_node; _ }: Syn.Cst.parenthesized_expression) ->
  Syn.Ceibo.Red.SyntaxNode.children syntax_node |> List.filter_map
    ~fn:(
      function
      | Syn.Ceibo.Red.Token token ->
          let text = Syn.Ceibo.Red.SyntaxToken.text token in
          if String.equal text " " || String.equal text "\n" || String.equal text "\t" then
            None
          else
            Some (String.equal text "begin")
      | _ -> None
    ) |> List.head |> Option.unwrap_or ~default:false

let is_obviously_redundant = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Parenthesized _ -> true
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _ -> false
  | _ -> false

let make_diagnostic = fun (expr: Syn.Cst.parenthesized_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(expr.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:"Remove these redundant parentheses."
    ~fix:(Fix.make
      ~title:"Remove redundant parentheses"
      ~operations:[
        Fix.replace_node
          ~target:expr.syntax_node
          ~replacement:(Syn.Cst.Expression.syntax_node expr.inner);
      ])
    ()

let rec diagnostics_for_expression = fun ~inside_redundant_chain ->
  function
  | Syn.Cst.Expression.Parenthesized expr ->
      let inner = expr.inner in
      let nested = diagnostics_for_expression
        ~inside_redundant_chain:(inside_redundant_chain || is_obviously_redundant inner)
        inner in
      if opens_with_begin expr then
        nested
      else if is_obviously_redundant inner && not inside_redundant_chain then
        make_diagnostic expr :: nested
      else
        nested
  | expr -> child_expressions expr
  |> List.map ~fn:(diagnostics_for_expression ~inside_redundant_chain:false)
  |> List.concat

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.map ~fn:Traversal.let_bindings_of_structure_item
  |> List.concat
  |> List.map
    ~fn:(fun binding ->
      diagnostics_for_expression ~inside_redundant_chain:false (Syn.Cst.LetBinding.value binding))
  |> List.concat

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
