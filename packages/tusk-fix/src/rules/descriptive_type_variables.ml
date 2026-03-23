open Std
open Std.Collections

let rule_id = "descriptive-type-variables"
let rule_description =
  "Type variables in type definitions should use descriptive names instead of short placeholders"

let rule_explain =
  {|
Avoid one-letter type variable names like 'a and 'b in real type definitions.

Why this rule exists:
- Short type variables are compact but not descriptive.
- In public APIs they force the reader to reverse-engineer intent from context.

What to do instead:
- Use names that communicate role.
- Prefer names like 'value, 'error, 'state, or 'msg when those roles matter.

Examples:
  Bad:    type ('a, 'b) resultish = ...
  Better: type ('value, 'error) resultish = ...
|}

let is_lower_alpha ch = ch >= 'a' && ch <= 'z'

let should_flag_type_variable_name name =
  String.length name = 1 && is_lower_alpha name.[0]

let make_diagnostic type_variable =
  let original = Syn.Cst.TypeVariable.text type_variable in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:
      (Syn.Cst.TypeVariable.syntax_node type_variable
      |> Syn.Ceibo.Red.SyntaxNode.span)
    ~suggestion:
      ("Prefer a descriptive type variable name instead of " ^ original
     ^ ", for example 'value or 'error")
    ()

let diagnostic_for_type_param = function
  | Syn.Cst.TypeParameter.{ type_variable = Some type_variable; _ } ->
      let name = Syn.Cst.TypeVariable.name type_variable in
      if should_flag_type_variable_name name then
        Some (make_diagnostic type_variable)
      else
        None
  | Syn.Cst.TypeParameter.{ type_variable = None; _ } -> None

let diagnostic_for_decl (Syn.Cst.TypeDeclaration.{ type_params; _ }) =
  type_params |> List.filter_map diagnostic_for_type_param

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostic_for_decl decl
           | _ -> [])
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostic_for_decl decl
           | _ -> [])

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file -> diagnostics_for_items source_file

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
