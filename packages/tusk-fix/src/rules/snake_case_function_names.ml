open Std
open Std.Collections

let rule_id = "snake-case-function-names"
let rule_name = "Snake Case Function Names"
let rule_code = "F0103"

let rule_description =
  "Function names should use snake_case instead of camelCase"

let rule_message =
  "Function names should use snake_case instead of camelCase."

let rule_explain =
  {|
Function names should use snake_case.

Why this rule exists:
- Snake case is the dominant value/function naming style across Riot.
- camelCase function names stick out immediately and make APIs feel imported rather than native.

Examples:
  Bad:    let parseUser input = ...
  Better: let parse_user input = ...
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

let should_flag_function_name text =
  not (String.equal text (to_snake_case text))

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostic_for_binding binding =
  if not (Syn.Cst.LetBinding.is_function binding) then
    None
  else
    let name = Syn.Cst.LetBinding.name binding in
    if should_flag_function_name name then
      let token =
        Syn.Cst.LetBinding.binding_name_token binding
        |> Syn.Cst.Token.syntax_token
      in
      Some (make_diagnostic token)
    else
      None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
