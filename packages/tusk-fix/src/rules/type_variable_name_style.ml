open Std
open Std.Collections

let rule_id = "descriptive-type-variables"
let rule_name = "Descriptive Type Variables"

let rule_description =
  "Type variables in type definitions should use descriptive names instead of short placeholders"

let is_lower_alpha ch = ch >= 'a' && ch <= 'z'

let should_flag_type_variable_name name =
  String.length name = 1 && is_lower_alpha name.[0]

let make_diagnostic type_variable =
  let original = Syn.Cst.TypeVariable.text type_variable in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known Diagnostic_code.ShortTypeVariableName)
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

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.items source_file
      |> List.concat_map (function
           | Syn.Cst.Item.TypeDeclaration decl -> diagnostic_for_decl decl
           | _ -> [])

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
