open Std
open Std.Collections

let rule_id = "no-inline-parameter-type-annotations"

let rule_description = "Function parameter type annotations should live in the function signature, not inline on each parameter"

let rule_explain = {|
Inline parameter annotations scatter a function signature across the argument list.
That makes the API harder to skim because readers have to reconstruct the type from
several small annotations instead of reading it in one place.

Putting the type on the binding itself keeps the interface intact:
`let render : int -> bool -> view = ...`.
The parameters can then stay focused on names, and the type stays focused on the
shape of the function.

This also makes later refactors easier, because the function already has a single
obvious place where its signature lives.
|}

let rec subtree_contains_kind = fun (node: Syn.Cst.syntax_node) target_kind ->
  if Syn.Ceibo.Red.SyntaxNode.kind node = target_kind then
    true
  else
    Syn.Ceibo.Red.SyntaxNode.children node |> Array.to_list |> List.exists
      (
        function
        | Syn.Ceibo.Red.Node child -> subtree_contains_kind child target_kind
        | Syn.Ceibo.Red.Token _ -> false
      )

let parameter_has_inline_type = fun parameter ->
  let node = Syn.Cst.Parameter.syntax_node parameter in
  subtree_contains_kind node Syn.SyntaxKind.TYPED_PATTERN
  || subtree_contains_kind node Syn.SyntaxKind.TYPE_CONSTRAINT

let make_diagnostic = fun parameter -> Diagnostic.make
~severity:Warning
~kind:(Diagnostic.Known {rule_id; message = rule_description})
~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Parameter.syntax_node parameter))
~suggestion:"Move the parameter type annotation into the function signature"
()

let diagnostic_for_binding = fun binding -> Syn.Cst.LetBinding.parameters binding
|> List.find_opt parameter_has_inline_type
|> Option.map make_diagnostic

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.filter Syn.Cst.LetBinding.is_function
  |> List.filter_map diagnostic_for_binding

let make = fun () -> Rule.make
~id:rule_id
~description:rule_description
~explain:rule_explain
~run:check_tree
()
