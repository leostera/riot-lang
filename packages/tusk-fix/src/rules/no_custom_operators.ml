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
  | Syn.Cst.Expression.PathExpression _
  | Syn.Cst.Expression.StringLiteral _
  | Syn.Cst.Expression.Unknown _ ->
      []
  | Syn.Cst.Expression.ApplyExpression expr ->
      diagnostics_for_expression (Syn.Cst.ApplyExpression.callee expr)
      @ diagnostics_for_expression (Syn.Cst.ApplyExpression.argument expr)
  | Syn.Cst.Expression.ParenthesizedExpression expr ->
      diagnostics_for_expression (Syn.Cst.ParenthesizedExpression.inner expr)
  | Syn.Cst.Expression.InfixExpression expr ->
      let nested =
        diagnostics_for_expression (Syn.Cst.InfixExpression.left expr)
        @ diagnostics_for_expression (Syn.Cst.InfixExpression.right expr)
      in
      if should_flag_operator (Syn.Cst.InfixExpression.operator expr) then
        make_diagnostic expr :: nested
      else
        nested

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
