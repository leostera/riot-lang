open Std
open Std.Collections

let rule_id = "no-prime-variables"

let rule_description = "Variable names should not contain apostrophes"

let rule_explain = {|
Apostrophes are compact, but they carry almost no meaning. `state'` tells the reader
that this binding is somehow related to `state`, but it does not say whether it is the
next value, an updated copy, a normalized version, or just a temporary alias.

Descriptive suffixes scale much better. Names like `next_state`, `updated_state`, or
`state2` may be a little longer, but they make data flow easier to follow without
relying on local convention.

This matters most in code that evolves over time. A prime that felt obvious when the
code had two lines often becomes opaque once the function grows.
|}

let contains_prime = fun text ->
  String.exists (fun ch -> ch = '\'') text

let replacement_for = fun text ->
  if String.equal text "" then
    text
  else if String.ends_with ~suffix:"'" text then
    String.sub text 0 (String.length text - 1) ^ "2"
  else
    String.map
      (fun ch ->
        if ch = '\'' then
          '2'
        else
          ch)
      text

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = replacement_for original in
  Diagnostic.make
  ~severity:Warning
  ~kind:(Diagnostic.Known {rule_id; message = rule_description})
  ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
  ~suggestion:(("Rename " ^ original ^ " to " ^ replacement))
  ()

let diagnostic_for_binding_site = fun (site:Traversal.binding_site) ->
  if site.is_function then
    None
  else
    let name = Syn.Cst.Token.text site.name_token in
    if contains_prime name then
      Some (make_diagnostic (Syn.Cst.Token.syntax_token site.name_token))
    else
      None

let check_tree = fun (ctx:Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.binding_sites_of_structure_item
  |> List.filter_map diagnostic_for_binding_site

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
