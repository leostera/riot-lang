open Std
open Std.Collections

let rule_id = "snake-case-argument-names"

let rule_description = "Argument names should use snake_case instead of camelCase"

let rule_explain = {|
Arguments live at the same value level as local bindings, record fields, and ordinary
function names. Using `snake_case` there keeps the language visually uniform.

camelCase arguments stand out as if they were imported from a different style system.
That visual mismatch is small in one function and surprisingly noisy across a whole
API surface, especially for named arguments that appear at every call site.

Keep parameter names in the same `snake_case` style as the rest of the value-level
language.
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

let should_flag_argument_name = fun text -> not (String.equal text (to_snake_case text))

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:(("Rename " ^ original ^ " to " ^ replacement))
    ()

let diagnostic_for_parameter = fun parameter ->
  match Syn.Cst.Parameter.name_token parameter with
  | Some token ->
      let name = Syn.Cst.Token.text token in
      if should_flag_argument_name name then
        Some (make_diagnostic (Syn.Cst.Token.syntax_token token))
      else
        None
  | None -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  Rule_query.let_bindings ctx
  |> List.filter Syn.Cst.LetBinding.is_function
  |> List.concat_map
    (fun binding -> Syn.Cst.LetBinding.parameters binding |> List.filter_map diagnostic_for_parameter)

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
