open Std

let rule_id = "no-useless-let-return"

let rule_description = "Bindings that immediately return the bound name should be collapsed"

let rule_explain = {|
A `let` binding should usually buy you something: a clearer name, a reused value, or a
place to hang additional logic. When the body immediately returns the same binding, the
extra name is not carrying its weight.

`let value = compute () in value` is longer than `compute ()` but does not explain the
code any better. In those cases, the direct expression is easier to read because it
shows the real work without the temporary detour.

Keep the binding when the name genuinely helps. Drop it when the name is created only
to be handed straight back.
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr -> unwrap_parens expr.inner
  | expr -> expr

let binding_name = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } -> Some (Syn.Cst.Token.text name_token)
  | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Literal _
  | Syn.Cst.Pattern.Parenthesized _ -> None
  | _ -> None

let body_name = function
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.Ident.name path
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Parenthesized _ -> None
  | _ -> None

let make_fix = fun (expr: Syn.Cst.let_expression) ->
  if expr.parameters != [] || Option.is_some expr.and_binding then
    None
  else
    Some (Fix.make
      ~title:"Replace this let-binding with its bound expression"
      ~operations:[
        Fix.replace
          ~target:(Fix.Node expr.syntax_node)
          ~replacement:(Fix.source_of_node (Syn.Cst.Expression.syntax_node expr.bound_value));
      ])

let make_diagnostic = fun (expr: Syn.Cst.let_expression) ->
  let fix = make_fix expr in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Replace this let-binding with its bound expression."
    ?fix
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Let expr -> (
      match binding_name expr.binding_pattern, body_name (unwrap_parens expr.body) with
      | Some let_name, Some body_name when String.equal let_name body_name -> Some (make_diagnostic expr)
      | _ -> None
    )
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.expressions_of_structure_item
  |> List.filter_map diagnostic_for_expression

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
