open Std

let rule_id = "prefer-sequences-over-let-unit"
let rule_name = "Prefer Sequences Over Let Unit"
let rule_code = "F0131"

let rule_description =
  "Effectful let-unit bindings should be written as `;` sequences"

let rule_message =
  "Replace let () = ... in ... with a `;` sequence."

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
  | Syn.Cst.Pattern.UnitPattern _ -> true
  | Syn.Cst.Pattern.ParenthesizedPattern { inner; _ } -> is_unit_pattern inner
  | Syn.Cst.Pattern.IdentifierPattern _ | Syn.Cst.Pattern.UnknownPattern _ -> false

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.LetExpression.syntax_node expr))
    ~suggestion:"Replace this let-unit binding with a `;` sequence."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.LetExpression expr
    when is_unit_pattern (Syn.Cst.LetExpression.binding_pattern expr) ->
      Some (make_diagnostic expr)
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
