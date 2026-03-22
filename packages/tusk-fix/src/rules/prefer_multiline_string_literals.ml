open Std

let rule_id = "prefer-multiline-string-literals"
let rule_name = "Prefer Multiline String Literals"
let rule_code = "F0119"

let rule_description =
  "String literal chains should use multiline string literals instead of repeated concatenation"

let rule_message =
  "String literal chains should use multiline string literals instead of repeated concatenation."

let rule_explain =
  {explain|
Repeatedly concatenating string literals should be avoided.

Why this rule exists:
- Long chains of `^` are harder to read than a single multiline literal.
- Multiline literals make surrounding punctuation and whitespace much easier to inspect.

Examples:
  Bad:    "hello " ^ "world" ^ "!"
  Better: {|
hello world!
|}
|explain}

let rec string_literal_chain_size = function
  | Syn.Cst.Expression.Path _ -> None
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) -> Some 1
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Apply _ ->
      None
  | Syn.Cst.Expression.Fun expr ->
      string_literal_chain_size (Syn.Cst.FunExpression.body expr)
  | Syn.Cst.Expression.Function expr ->
      Syn.Cst.FunctionExpression.cases expr
      |> List.find_map (fun case ->
             match Syn.Cst.MatchCase.guard case with
             | Some guard -> (
                 match string_literal_chain_size guard with
                 | Some _ as size -> size
                 | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case))
             | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case))
  | Syn.Cst.Expression.Parenthesized expr ->
      string_literal_chain_size (Syn.Cst.ParenthesizedExpression.inner expr)
  | Syn.Cst.Expression.Infix expr
    when String.equal (Syn.Cst.InfixExpression.operator expr) "^" -> (
      match
        string_literal_chain_size (Syn.Cst.InfixExpression.left expr),
        string_literal_chain_size (Syn.Cst.InfixExpression.right expr)
      with
      | Some left_count, Some right_count -> Some (left_count + right_count)
      | _ -> None)
  | Syn.Cst.Expression.Let expr -> (
      match string_literal_chain_size (Syn.Cst.LetExpression.bound_value expr) with
      | Some _ as size -> size
      | None -> string_literal_chain_size (Syn.Cst.LetExpression.body expr))
  | Syn.Cst.Expression.Match expr -> (
      match string_literal_chain_size (Syn.Cst.MatchExpression.scrutinee expr) with
      | Some _ as size -> size
      | None ->
          Syn.Cst.MatchExpression.cases expr
          |> List.find_map (fun case ->
                 match Syn.Cst.MatchCase.guard case with
                 | Some guard -> (
                     match string_literal_chain_size guard with
                     | Some _ as size -> size
                     | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case))
                 | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case)))
  | Syn.Cst.Expression.Try expr -> (
      match string_literal_chain_size (Syn.Cst.TryExpression.body expr) with
      | Some _ as size -> size
      | None ->
          Syn.Cst.TryExpression.cases expr
          |> List.find_map (fun case ->
                 match Syn.Cst.MatchCase.guard case with
                 | Some guard -> (
                     match string_literal_chain_size guard with
                     | Some _ as size -> size
                     | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case))
                 | None -> string_literal_chain_size (Syn.Cst.MatchCase.body case)))
  | Syn.Cst.Expression.If expr -> (
      match string_literal_chain_size (Syn.Cst.IfExpression.then_branch expr) with
      | Some _ as size -> size
      | None -> (
          match Syn.Cst.IfExpression.else_branch expr with
          | Some else_branch -> string_literal_chain_size else_branch
          | None -> None))
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Unknown _ ->
      None

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:"Use a multiline string literal like {| ... |} instead of concatenating string literals"
    ()

let diagnostic_for_binding binding =
  match string_literal_chain_size (Syn.Cst.LetBinding.value binding) with
  | Some count when count >= 2 ->
      Some (make_diagnostic (Syn.Cst.LetBinding.value binding))
  | Some _ | None ->
      None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
