open Std

let rule_id = "no-custom-operators"
let rule_name = "No Custom Operators"
let rule_code = "F0120"

let rule_description =
  "Custom infix operators should be avoided in favor of named functions"

let rule_message =
  "Custom infix operators should be avoided in favor of named functions."

let rule_explain =
  {|
Custom infix operators should be avoided.

Why this rule exists:
- Symbolic operators are hard to search for and easy to misread.
- Named functions communicate intent much better than custom punctuation.

Examples:
  Bad:    value %> next
  Better: compose_right value next
|}

let allowed_infix_operators =
  [
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

let should_flag_operator operator =
  not (List.mem operator allowed_infix_operators)

let make_diagnostic expr =
  let operator = Syn.Cst.InfixExpression.operator expr in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Cst.InfixExpression.operator_token expr |> Syn.Cst.Token.span)
    ~suggestion:("Replace " ^ operator ^ " with a named function")
    ()

let rec diagnostics_for_expression = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _ ->
      []
  | Syn.Cst.Expression.Apply expr ->
      diagnostics_for_expression (Syn.Cst.ApplyExpression.callee expr)
      @
      (match Syn.Cst.ApplyExpression.argument expr with
      | Syn.Cst.Positional argument -> diagnostics_for_expression argument
      | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } -> (
          match value with
          | Some value -> diagnostics_for_expression value
          | None -> []))
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      diagnostics_for_expression receiver
  | Syn.Cst.Expression.Fun expr ->
      diagnostics_for_expression (Syn.Cst.FunExpression.body expr)
  | Syn.Cst.Expression.Function expr ->
      Syn.Cst.FunctionExpression.cases expr
      |> List.concat_map (fun case ->
             (match Syn.Cst.MatchCase.guard case with
             | Some guard -> diagnostics_for_expression guard
             | None -> [])
             @ diagnostics_for_expression (Syn.Cst.MatchCase.body case))
  | Syn.Cst.Expression.Parenthesized expr ->
      diagnostics_for_expression (Syn.Cst.ParenthesizedExpression.inner expr)
  | Syn.Cst.Expression.Let expr ->
      diagnostics_for_expression (Syn.Cst.LetExpression.bound_value expr)
      @ diagnostics_for_expression (Syn.Cst.LetExpression.body expr)
  | Syn.Cst.Expression.Match expr ->
      diagnostics_for_expression (Syn.Cst.MatchExpression.scrutinee expr)
      @
      (Syn.Cst.MatchExpression.cases expr
      |> List.concat_map (fun case ->
             (match Syn.Cst.MatchCase.guard case with
             | Some guard -> diagnostics_for_expression guard
             | None -> [])
             @ diagnostics_for_expression (Syn.Cst.MatchCase.body case)))
  | Syn.Cst.Expression.Try expr ->
      diagnostics_for_expression (Syn.Cst.TryExpression.body expr)
      @
      (Syn.Cst.TryExpression.cases expr
      |> List.concat_map (fun case ->
             (match Syn.Cst.MatchCase.guard case with
             | Some guard -> diagnostics_for_expression guard
             | None -> [])
             @ diagnostics_for_expression (Syn.Cst.MatchCase.body case)))
  | Syn.Cst.Expression.If expr ->
      diagnostics_for_expression (Syn.Cst.IfExpression.condition expr)
      @ diagnostics_for_expression (Syn.Cst.IfExpression.then_branch expr)
      @
      (match Syn.Cst.IfExpression.else_branch expr with
      | Some else_branch -> diagnostics_for_expression else_branch
      | None -> [])
  | Syn.Cst.Expression.Infix expr ->
      let nested =
        diagnostics_for_expression (Syn.Cst.InfixExpression.left expr)
        @ diagnostics_for_expression (Syn.Cst.InfixExpression.right expr)
      in
      if should_flag_operator (Syn.Cst.InfixExpression.operator expr) then
        make_diagnostic expr :: nested
      else
        nested
  | _ ->
      []

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.concat_map (fun binding ->
             diagnostics_for_expression (Syn.Cst.LetBinding.value binding))

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
