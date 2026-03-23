open Std

let rule_id = "prefer-pipelines-for-nested-calls"
let rule_description =
  "Deeply nested function calls should usually be written as pipelines"

let rule_explain =
  {|
Deeply nested function calls are easier to read when written as pipelines.

Why this rule exists:
- `foo (bar (baz (hex 1)))` forces the reader to scan inside-out.
- Pipelines read in execution order and make each transformation step explicit.
- Once a call chain gets deep enough, `|>` is usually easier on the eyes.

Examples:
  Bad:    foo (bar (baz (hex 1)))
  Better: hex 1 |> baz |> bar |> foo
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_parens expr.inner
  | expr -> expr

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } -> value

let rec nested_call_count expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply apply ->
      (match expression_of_apply_argument apply.argument with
      | Some argument -> 1 + nested_call_count argument
      | None -> 1)
  | _ -> 0

let threshold = 4

let make_diagnostic expr =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:"Rewrite this call chain as a pipeline."
    ()

let diagnostic_for_expression expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply _ when nested_call_count expr >= threshold ->
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
