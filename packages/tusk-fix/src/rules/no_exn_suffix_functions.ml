open Std
open Std.Collections

let rule_id = "no-exn-suffix-functions"

let rule_description = "Function names should not end with _exn"

let rule_explain = {|
Names ending in `_exn` normalize exception-throwing control flow as part of the API.
That makes failure behavior harder to reason about at call sites and encourages
exceptions for situations that are often expected, such as parse failure or lookup
failure.

Prefer APIs that return `Result` or `Option` and make failure explicit in the type.
If an exceptional variant truly has to exist, it should usually be the less prominent
entry point rather than the one that shapes the naming of the whole interface.

The goal here is not just naming. It is to steer APIs away from using exceptions as
ordinary control flow.
|}

let should_flag_binding_site = fun (site: Traversal.binding_site) ->
  site.is_function && String.ends_with ~suffix:"_exn" (Syn.Cst.Token.text site.name_token)

let make_diagnostic = fun (site: Traversal.binding_site) ->
  let name = Syn.Cst.Token.text site.name_token in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Cst.Token.syntax_token site.name_token |> Syn.Ceibo.Red.SyntaxToken.span)
    ~suggestion:("Rename " ^ name ^ " to remove the _exn suffix and prefer a Result/Option API.")
    ()

let diagnostic_for_binding_site = fun (site: Traversal.binding_site) ->
  if should_flag_binding_site site then
    Some (make_diagnostic site)
  else
    None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.binding_sites_of_structure_item
  |> List.filter_map diagnostic_for_binding_site

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
