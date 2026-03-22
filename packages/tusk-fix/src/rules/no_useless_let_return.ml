open Std

let rule_id = "no-useless-let-return"
let rule_name = "No Useless Let Return"
let rule_code = "F0133"

let rule_description =
  "Bindings that immediately return the bound name should be collapsed"

let rule_message =
  "Replace let bindings that immediately return the bound name with the bound expression."

let rule_explain =
  {|
Bindings that immediately return the bound name should be collapsed to the bound expression.

Why this rule exists:
- `let value = compute () in value` adds a name but does not add information.
- The extra binding makes the expression longer without changing control flow or meaning.
- If the binding exists only to hand the same value back immediately, the simpler expression is easier to read.

Examples:
  Bad:    let value = load_config () in value
  Better: load_config ()

  Bad:    let result = parse input in result
  Better: parse input
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_parens expr.inner
  | expr -> expr

let binding_name = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } -> Some (Syn.Cst.Token.text name_token)
  | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Literal _
  | Syn.Cst.Pattern.Parenthesized _ ->
      None
  | _ ->
      None

let body_name = function
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.ModulePath.name path
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Parenthesized _ ->
      None
  | _ ->
      None

let make_diagnostic (expr : Syn.Cst.let_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Replace this let-binding with its bound expression."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Let expr -> (
      match
        binding_name expr.binding_pattern,
        body_name (unwrap_parens expr.body)
      with
      | Some let_name, Some body_name when String.equal let_name body_name ->
          Some (make_diagnostic expr)
      | _ -> None)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
