open Std

let rule_id = "no-custom-operators"

let rule_description = "Custom infix operators should be avoided in favor of named functions"

let rule_explain = {|
Custom symbolic operators trade a small amount of local brevity for a lot of global
guesswork. Unless the operator is already conventional, readers have to stop and
remember what `%>`, `>>?`, or `<+>` means before they can follow the expression.

They are also unpleasant to search for, awkward to discuss in review, and easy to
confuse with similar punctuation from other libraries.

A named function usually tells the story directly. `compose_right value next` may be
longer than `value %> next`, but it is obvious to readers who have never seen the
code before.
|}

let allowed_infix_operators = [
  "=";
  "<>";
  "!=";
  "<";
  ">";
  "<=";
  ">=";
  "&&";
  "||";
  "+";
  "-";
  "*";
  "/";
  "+.";
  "-.";
  "*.";
  "/.";
  "mod";
  "land";
  "lor";
  "lxor";
  "lsl";
  "lsr";
  "asr";
  "::";
  "@";
  "^";
  "|>";
  "@@";

]

let should_flag_operator = fun operator -> not (List.mem operator allowed_infix_operators)

let make_diagnostic = fun expr ->
  let operator = Syn.Cst.InfixExpression.operator expr in
  Diagnostic.make
  ~severity:Warning
  ~kind:(Diagnostic.Known {rule_id; message = rule_description})
  ~span:(((((Syn.Cst.InfixExpression.operator_token expr |> Syn.Cst.Token.span)))))
  ~suggestion:((((("Replace " ^ operator ^ " with a named function")))))
  ()

let rec diagnostics_for_expression =
  function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      diagnostics_for_expression expr.callee @ (
        match expr.argument with
        | Syn.Cst.Positional argument -> diagnostics_for_expression argument
        | Syn.Cst.Labeled { value; _ }
        | Syn.Cst.Optional { value; _ } -> (
            match value with
            | Some value -> diagnostics_for_expression value
            | None -> []
          )
      )
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      diagnostics_for_expression receiver
  | Syn.Cst.Expression.Fun expr ->
      diagnostics_for_function_body expr.body
  | Syn.Cst.Expression.Function { syntax_node; cases; _ } ->
      diagnostics_for_function_body (Syn.Cst.Cases {syntax_node; cases})
  | Syn.Cst.Expression.Parenthesized expr ->
      diagnostics_for_expression expr.inner
  | Syn.Cst.Expression.Let expr ->
      diagnostics_for_expression expr.bound_value @ diagnostics_for_expression expr.body
  | Syn.Cst.Expression.Match expr ->
      diagnostics_for_expression expr.scrutinee @ (
        expr.cases |> List.concat_map
          (fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> diagnostics_for_expression guard
              | None -> []
            ) @ diagnostics_for_expression case.body)
      )
  | Syn.Cst.Expression.Try expr ->
      diagnostics_for_expression expr.body @ (
        expr.cases |> List.concat_map
          (fun (case: Syn.Cst.match_case) ->
            (
              match case.guard with
              | Some guard -> diagnostics_for_expression guard
              | None -> []
            ) @ diagnostics_for_expression case.body)
      )
  | Syn.Cst.Expression.If expr ->
      diagnostics_for_expression expr.condition @ diagnostics_for_expression expr.then_branch @ (
        match expr.else_branch with
        | Some else_branch -> diagnostics_for_expression else_branch
        | None -> []
      )
  | Syn.Cst.Expression.Infix expr ->
      let nested = diagnostics_for_expression (Syn.Cst.InfixExpression.left expr)
      @ diagnostics_for_expression (Syn.Cst.InfixExpression.right expr) in
      if should_flag_operator (Syn.Cst.InfixExpression.operator expr) then
        make_diagnostic expr :: nested
      else
        nested
  | _ ->
      []
and diagnostics_for_function_body =
  function
  | Syn.Cst.Expression expression -> diagnostics_for_expression expression
  | Syn.Cst.Cases { cases; _ } -> cases |> List.concat_map diagnostics_for_match_case
and diagnostics_for_match_case = fun (case: Syn.Cst.match_case) ->
  (
    match case.guard with
    | Some guard -> diagnostics_for_expression guard
    | None -> []
  ) @ diagnostics_for_expression case.body

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.concat_map (fun binding -> diagnostics_for_expression (Syn.Cst.LetBinding.value binding))

let make = fun () -> Rule.make
~id:rule_id
~description:rule_description
~explain:rule_explain
~run:check_tree
()
