open Std

let rule_id = "no-eta-expansion"
let rule_name = "No Eta Expansion"
let rule_code = "F0137"

let rule_description =
  "Eta-expanded functions should be replaced by the function they call"

let rule_message =
  "Eta-expanded functions should be replaced by the function they call."

let rule_explain =
  {|
Avoid eta-expanded wrappers like `fun x -> foo x`.

These wrappers do not add behavior.
They only forward their arguments to another function in the same order, which makes the code longer without changing meaning.
When the wrapper is just a pass-through, use the callee directly.

Examples:
  Avoid:   fun value -> render value
  Avoid:   fun left right -> compare left right
  Better:  render
  Better:  compare
|}

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

let rec expression_mentions_any_name names expr =
  match expr with
  | Syn.Cst.Expression.Path { path; _ } -> (
      match Syn.Cst.ModulePath.name path with
      | Some name -> List.mem name names
      | None -> false)
  | _ ->
      child_expressions expr
      |> List.exists (expression_mentions_any_name names)

let rec flatten_apply expr =
  match expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let flattened_callee, args = flatten_apply callee in
      let maybe_argument =
        match argument with
        | Syn.Cst.Positional argument -> Some argument
        | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } -> value
      in
      (match maybe_argument with
      | Some argument -> flattened_callee, args @ [ argument ]
      | None -> flattened_callee, args)
  | _ -> expr, []

let path_name = function
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.ModulePath.name path
  | _ -> None

let positional_parameter_names parameters =
  let rec gather acc = function
    | [] -> Some (List.rev acc)
    | parameter :: rest -> (
        match parameter with
        | Syn.Cst.Parameter.Positional _ -> (
            match Syn.Cst.Parameter.name parameter with
            | Some name -> gather (name :: acc) rest
            | None -> None)
        | Syn.Cst.Parameter.Labeled _
        | Syn.Cst.Parameter.Optional _
        | Syn.Cst.Parameter.LocallyAbstract _ ->
            None)
  in
  gather [] parameters

let rec parameter_arguments_match parameter_names arguments =
  match parameter_names, arguments with
  | [], [] -> true
  | parameter_name :: rest_names, argument :: rest_arguments -> (
      match path_name argument with
      | Some argument_name ->
          String.equal parameter_name argument_name
          && parameter_arguments_match rest_names rest_arguments
      | None -> false)
  | _, _ -> false

let should_flag_fun (expr : Syn.Cst.fun_expression) =
  match positional_parameter_names expr.parameters with
  | None | Some [] -> false
  | Some parameter_names -> (
      match expr.body with
      | Syn.Cst.Cases _ ->
          false
      | Syn.Cst.Expression body ->
          let callee, arguments = flatten_apply body in
          parameter_arguments_match parameter_names arguments
          && not (expression_mentions_any_name parameter_names callee))

let make_diagnostic (expr : Syn.Cst.fun_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(expr.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:"Replace this eta-expanded function with the callee directly."
    ()

let rec diagnostic_for_expression = function
  | Syn.Cst.Expression.Fun expr when should_flag_fun expr ->
      [ make_diagnostic expr ]
  | expr ->
      child_expressions expr
      |> List.concat_map diagnostic_for_expression

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.concat_map (fun binding ->
             diagnostic_for_expression (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
