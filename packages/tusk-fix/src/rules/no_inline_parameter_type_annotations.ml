open Std
open Std.Collections

let rule_id = "no-inline-parameter-type-annotations"
let rule_description =
  "Function parameter type annotations should live in the function signature, not inline on each parameter"

let rule_explain =
  {|
Inline parameter type annotations should be avoided in function definitions.

Why this rule exists:
- Function signatures are easier to scan when the full type lives in one place.
- Inline parameter annotations make it harder to evolve a function into a signed `let name : ... = ...` form.

Examples:
  Bad:    let render (user_id : int) (enabled : bool) = ...
  Better: let render : int -> bool -> view = fn user_id enabled -> ...
|}

let rec subtree_contains_kind (node : Syn.Cst.syntax_node) target_kind =
  if Syn.Ceibo.Red.SyntaxNode.kind node = target_kind then
    true
  else
    Syn.Ceibo.Red.SyntaxNode.children node
    |> Array.to_list
    |> List.exists (function
         | Syn.Ceibo.Red.Node child -> subtree_contains_kind child target_kind
         | Syn.Ceibo.Red.Token _ -> false)

let parameter_has_inline_type parameter =
  let node = Syn.Cst.Parameter.syntax_node parameter in
  subtree_contains_kind node Syn.SyntaxKind.TYPED_PATTERN
  || subtree_contains_kind node Syn.SyntaxKind.TYPE_CONSTRAINT

let make_diagnostic parameter =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Parameter.syntax_node parameter))
    ~suggestion:"Move the parameter type annotation into the function signature"
    ()

let diagnostic_for_binding binding =
  Syn.Cst.LetBinding.parameters binding
  |> List.find_opt parameter_has_inline_type
  |> Option.map make_diagnostic

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
