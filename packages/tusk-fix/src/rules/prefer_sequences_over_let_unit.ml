open Std

let rule_id = "prefer-sequences-over-let-unit"
let rule_description =
  "Effectful let-unit bindings should be written as `;` sequences"

let rule_explain =
  {|
Effectful `let () = ... in ...` expressions should usually be written as sequences.

Why this rule exists:
- `let () = side_effect () in next ()` is sequencing in disguise.
- `side_effect (); next ()` makes the control flow obvious immediately.
- The `let () =` form is best saved for places where the unit pattern itself matters.

Examples:
  Bad:    let () = log () in flush ()
  Better: log (); flush ()
|}

let rec is_unit_pattern = function
  | Syn.Cst.Pattern.Literal { literal = Syn.Cst.PatternLiteral.Unit _; _ } -> true
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> is_unit_pattern inner
  | Syn.Cst.Pattern.Identifier _ | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Literal _ ->
      false
  | _ ->
      false

let make_diagnostic (expr : Syn.Cst.let_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Replace this let-unit binding with a `;` sequence."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Let expr
    when is_unit_pattern expr.binding_pattern ->
      Some (make_diagnostic expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.expressions_of_structure_item
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
