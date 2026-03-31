open Std

let rule_id = "avoid-single-letter-function-names"

let rule_description = "Function names should be descriptive instead of using single-letter placeholders"

let rule_explain = {|
Single-letter function names tend to survive long after the code stopped being a toy.
Once that happens, every caller has to recover the meaning of `f`, `g`, or `h` from
surrounding context instead of from the API itself.

This is especially painful in stack traces, grep results, and module interfaces,
where the function name may be the only clue the reader gets.

Write the smallest name that still carries the job of the function.
`parse_user`, `render_error`, and `normalize_path` all age much better than `f`.
|}

let should_flag_function_name = fun name -> String.length name = 1

let make_diagnostic = fun token ->
    let original = Syn.Ceibo.Red.SyntaxToken.text token in
    Diagnostic.make
      ~severity:Warning
      ~kind:(Diagnostic.Known {rule_id; message = rule_description})
      ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
      ~suggestion:(("Rename " ^ original ^ " to a descriptive function name"))
      ()

let diagnostic_for_binding_site = fun (site: Traversal.binding_site) ->
    if not site.is_function then
      None
    else
      let name = Syn.Cst.Token.text site.name_token in
      if should_flag_function_name name then
        Some (make_diagnostic (Syn.Cst.Token.syntax_token site.name_token))
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
