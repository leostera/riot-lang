open Std

let rule_id = "prefer-sequences-over-let-unit"

let rule_description = "Effectful let-unit bindings should be written as `;` sequences"

let rule_explain = {|
`let () = side_effect () in next ()` is usually just sequencing spelled in a heavier
way. The unit pattern is not carrying information there; it is only forcing the reader
to parse a `let` where a plain sequence would have said the same thing more directly.

`side_effect (); next ()` makes the flow obvious immediately: do this, then do that.
Reserve `let () = ... in ...` for the places where binding the unit result is part of
the point, not for ordinary effect sequencing.

This rule exists because unit-binding syntax can make straightforward imperative steps
look more abstract than they really are.
|}

let rec is_unit_pattern = function
  | Syn.Cst.Pattern.Literal { literal=Syn.Cst.PatternLiteral.Unit _; _ } -> true
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> is_unit_pattern inner
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Literal _ -> false
  | _ -> false

let source_slice = fun ~source span ->
  let len = Syn.Ceibo.Span.(span.end_ - span.start) in
  String.sub source span.start len

let source_of_node = fun ~source node ->
  source_slice ~source (Syn.Ceibo.Red.SyntaxNode.span node)

let leading_trivia_source = fun ~source node ->
  let tokens =
    Traversal.find_tokens
      (fun token -> not (Traversal.is_trivia (Syn.Ceibo.Red.SyntaxToken.kind token)))
      node
  in
  match tokens with
  | [] -> ""
  | first :: _ ->
      let node_span = Syn.Ceibo.Red.SyntaxNode.span node in
      let first_span = Syn.Ceibo.Red.SyntaxToken.span first in
      source_slice ~source (Syn.Ceibo.Span.make ~start:node_span.start ~end_:first_span.start)

let expression_source = fun ~source expr -> source_of_node ~source (Syn.Cst.Expression.syntax_node expr)

let sequence_separator = fun body ->
  if String.equal body "" then
    "; "
  else
    match body.[0] with
    | ' '
    | '\t'
    | '\n'
    | '\r' -> ";"
    | _ -> "; "

let make_fix = fun ~source (expr: Syn.Cst.let_expression) ->
  let leading = leading_trivia_source ~source expr.syntax_node in
  let bound_value = expression_source ~source expr.bound_value
    |> String.trim
    |> (fun source -> "(" ^ source ^ ")")
  in
  let body = expression_source ~source expr.body in
  Fix.make
    ~title:"Replace let-unit binding with a sequence"
    ~operations:[
      Fix.replace_node_with_text
        ~target:expr.syntax_node
        ~text:((leading ^ bound_value ^ sequence_separator body ^ body));
    ]

let make_diagnostic = fun ~source (expr: Syn.Cst.let_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Replace this let-unit binding with a `;` sequence."
    ~fix:(make_fix ~source expr)
    ()

let diagnostic_for_expression = fun ~source ->
  function
  | Syn.Cst.Expression.Let expr when is_unit_pattern expr.binding_pattern ->
      Some (make_diagnostic ~source expr)
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  Syn.Visit.source_file
    {
      Syn.Visit.default
      with visit_expression =
        (fun diagnostics walk expression ->
          let diagnostics =
            match diagnostic_for_expression ~source:ctx.source expression with
            | Some diagnostic -> diagnostic :: diagnostics
            | None -> diagnostics
          in
          walk.descend_expression diagnostics expression);
    }
    []
    ctx.cst |> List.rev

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
