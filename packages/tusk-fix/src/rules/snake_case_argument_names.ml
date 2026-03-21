open Std
open Std.Collections

let rule_id = "snake-case-argument-names"
let rule_name = "Snake Case Argument Names"
let rule_code = "F0110"

let rule_description =
  "Argument names should use snake_case instead of camelCase"

let rule_message =
  "Argument names should use snake_case instead of camelCase."

let rule_explain =
  {|
Argument names should use snake_case.

Why this rule exists:
- Named and positional parameters should read like the rest of the value-level language.
- camelCase arguments look like a different style system inside otherwise consistent functions.

Examples:
  Bad:    let create ~userId ~displayName = ...
  Better: let create ~user_id ~display_name = ...
|}

let is_upper ch = ch >= 'A' && ch <= 'Z'
let is_lower ch = ch >= 'a' && ch <= 'z'
let is_digit ch = ch >= '0' && ch <= '9'

let to_snake_case text =
  let pieces = ref [] in
  let push piece = pieces := piece :: !pieces in
  let prev_was_lower_or_digit = ref false in
  String.iter
    (fun ch ->
      if is_upper ch then (
        if !prev_was_lower_or_digit then push "_";
        push (String.make 1 (Char.lowercase_ascii ch));
        prev_was_lower_or_digit := false)
      else (
        push (String.make 1 ch);
        prev_was_lower_or_digit := is_lower ch || is_digit ch))
    text;
  String.concat "" (List.rev !pieces)

let should_flag_argument_name text =
  not (String.equal text (to_snake_case text))

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostic_for_parameter parameter =
  match Syn.Cst.Parameter.name_token parameter with
  | Some token ->
      let name = Syn.Cst.Token.text token in
      if should_flag_argument_name name then
        Some (make_diagnostic (Syn.Cst.Token.syntax_token token))
      else
        None
  | None -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.concat_map (fun binding ->
             Syn.Cst.LetBinding.parameters binding
             |> List.filter_map diagnostic_for_parameter)

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
