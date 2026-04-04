open Std
open Std.Collections

let rule_id = "snake-case-variable-names"

let rule_description = "Variable names should use snake_case instead of camelCase"

let rule_explain = {|
Local bindings are part of the ordinary value language, so they should follow the same
`snake_case` convention as function names, arguments, and record fields.

That consistency pays off in dense code. Pattern matches, `let` chains, and small local
helpers are easier to scan when the naming style does not keep changing between
bindings.

Use `current_user`, `next_state`, and `parsed_response` rather than mixing in
camelCase at the local level.
|}

let is_upper = fun ch -> ch >= 'A' && ch <= 'Z'

let is_lower = fun ch -> ch >= 'a' && ch <= 'z'

let is_digit = fun ch -> ch >= '0' && ch <= '9'

let to_snake_case = fun text ->
  let pieces = ref [] in
  let push piece =
    pieces := piece :: !pieces
  in
  let prev_was_lower_or_digit = ref false in
  String.iter
    (fun ch ->
      if is_upper ch then
        (
          if !prev_was_lower_or_digit then
            push "_";
          push (String.make 1 (Char.lowercase_ascii ch));
          prev_was_lower_or_digit := false
        )
      else (
        push (String.make 1 ch);
        prev_was_lower_or_digit := is_lower ch || is_digit ch
      ))
    text;
  String.concat "" (List.rev !pieces)

let should_flag_variable_name = fun text -> not (String.equal text (to_snake_case text))

let make_fix = fun token replacement ->
  Fix.make
    ~title:(("Rename variable " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " to " ^ replacement))
    ~operations:[ Fix.replace_token_with_text ~target:token ~text:replacement; ]

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:(("Rename " ^ original ^ " to " ^ replacement))
    ~fix:(make_fix token replacement)
    ()

let diagnostic_for_binding_site = fun (site: Traversal.binding_site) ->
  if site.is_function then
    None
  else
    let name = Syn.Cst.Token.text site.name_token in
    if should_flag_variable_name name then
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
