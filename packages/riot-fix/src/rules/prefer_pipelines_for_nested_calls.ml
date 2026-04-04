open Std

let rule_id = "prefer-pipelines-for-nested-calls"

let rule_description = "Deeply nested function calls should usually be written as pipelines"

let rule_explain = {|
Nested calls read from the inside out. That is manageable for one or two levels, but
once the chain gets deeper the reader has to bounce back and forth across the line to
reconstruct evaluation order.

Pipelines flatten the same flow into execution order: start with a value, apply one
transformation, then the next, then the next. That makes the intermediate steps easier
to spot and gives each transformation a stable visual rhythm.

This rule exists for the cases where nested calls have become dense enough that a
pipeline would make the same logic easier to follow.
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized expr -> unwrap_parens expr.inner
  | expr -> expr

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ }
  | Syn.Cst.Optional { value; _ } -> value

let rec nested_call_count = fun expr ->
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply apply -> (
      match expression_of_apply_argument apply.argument with
      | Some argument -> 1 + nested_call_count argument
      | None -> 1
    )
  | _ -> 0

let threshold = 4

let rec pipeline_parts = fun expr ->
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply { callee; argument=Syn.Cst.Positional argument; _ } -> (
      match pipeline_parts argument with
      | Some (seed, stages) -> Some (seed, stages @ [ callee ])
      | None -> Some (argument, [ callee ])
    )
  | _ -> None

let pipeline_text = fun expr ->
  match pipeline_parts expr with
  | None -> None
  | Some (seed, stages) ->
      let seed = Rule_text.expression seed in
      let stages = stages |> List.map Rule_text.expression in
      Some (String.concat " |> " (seed :: stages))

let make_diagnostic = fun expr ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:"Rewrite this call chain as a pipeline."
    ?fix:
      (match pipeline_text expr with
      | None -> None
      | Some text ->
          Some
            (Fix.make
               ~title:"Rewrite nested calls as a pipeline"
               ~operations:
                 [
                   Fix.replace_node_with_text
                     ~target:(Syn.Cst.Expression.syntax_node expr)
                     ~text:(" " ^ text);
                 ]))
    ()

let diagnostic_for_expression = fun expr ->
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply _ when nested_call_count expr >= threshold -> Some (make_diagnostic expr)
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.expressions_of_structure_item
  |> List.filter_map diagnostic_for_expression

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
