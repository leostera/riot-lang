open Std
open Std.Collections

let rule_id = "descriptive-type-variables"

let rule_description = "Type variables in type definitions should use descriptive names instead of short placeholders"

let rule_explain = {|
One-letter type variables are fine in tiny local examples, but they age badly in real
APIs. By the time a type reaches an interface or a diagnostic message, `'a` and `'b`
force the reader to reconstruct the meaning from the rest of the declaration.

Prefer names that tell the reader what role each variable plays. `'value`, `'error`,
`'state`, `'msg`, and `'key` are longer, but they let the type explain itself.

For example, `('value, 'error) resultish` tells a much clearer story than
`('a, 'b) resultish`, especially once the type appears in multiple modules.
|}

let is_lower_alpha = fun ch -> ch >= 'a' && ch <= 'z'

let should_flag_type_variable_name = fun name -> String.length name = 1 && is_lower_alpha name.[0]

let make_diagnostic = fun type_variable ->
  let original = Syn.Cst.TypeVariable.text type_variable in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:((Syn.Cst.TypeVariable.syntax_node type_variable |> Syn.Ceibo.Red.SyntaxNode.span))
    ~suggestion:(("Prefer a descriptive type variable name instead of " ^ original ^ ", for example 'value or 'error"))
    ()

let diagnostic_for_type_param = function
  | Syn.Cst.TypeParameter.{ type_variable=Some type_variable; _ } ->
      let name = Syn.Cst.TypeVariable.name type_variable in
      if should_flag_type_variable_name name then
        Some (make_diagnostic type_variable)
      else
        None
  | Syn.Cst.TypeParameter.{ type_variable=None; _ } -> None

let diagnostic_for_decl = fun (Syn.Cst.TypeDeclaration.{ type_params; _ }) ->
  type_params |> List.filter_map diagnostic_for_type_param

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.concat_map
        (
          function
          | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostic_for_decl decl
          | _ -> []
        )
  | Syn.Cst.Interface { items; _ } ->
      items |> List.concat_map
        (
          function
          | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostic_for_decl decl
          | _ -> []
        )

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_items source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
