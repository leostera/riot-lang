open Std
open Std.Collections

let rule_id = "no-exn-suffix-functions"
let rule_description =
  "Function names should not end with _exn"

let rule_explain =
  {|
Avoid naming functions with an `_exn` suffix.

Names like `parse_exn` and `getenv_exn` advertise exception-throwing control flow as part of the normal API surface.
That makes call sites harder to reason about and nudges callers toward exceptions for expected failure cases.
Prefer `Result` or `Option` returning functions, and reserve exceptions for truly exceptional situations.

Examples:
  Avoid:   let parse_exn text = ...
  Better:  let parse text = ...
  Better:  let parse_result text = ...
|}

let should_flag_binding_site (site : Traversal.binding_site) =
  site.is_function
  && String.ends_with ~suffix:"_exn" (Syn.Cst.Token.text site.name_token)

let make_diagnostic (site : Traversal.binding_site) =
  let name = Syn.Cst.Token.text site.name_token in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Cst.Token.syntax_token site.name_token |> Syn.Ceibo.Red.SyntaxToken.span)
    ~suggestion:
      ("Rename " ^ name
     ^ " to remove the _exn suffix and prefer a Result/Option API.")
    ()

let diagnostic_for_binding_site (site : Traversal.binding_site) =
  if should_flag_binding_site site then
    Some (make_diagnostic site)
  else
    None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.binding_sites_of_structure_item
      |> List.filter_map diagnostic_for_binding_site

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
