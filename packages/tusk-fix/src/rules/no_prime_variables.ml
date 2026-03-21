open Std
open Std.Collections

let rule_id = "no-prime-variables"
let rule_name = "No Prime Variables"
let rule_code = "F0106"

let rule_description =
  "Variable names should not contain apostrophes"

let rule_message =
  "Avoid apostrophes in variable names; prefer a descriptive suffix."

let rule_explain =
  {|
Avoid apostrophes in variable names.

Why this rule exists:
- Prime-suffixed names are compact but vague.
- Names like x' or state' force the reader to guess whether the binding is an update, a copy, or just a temporary.

What to do instead:
- Use a descriptive suffix like _next, _updated, or a numeric suffix like x2 when that is genuinely the best name.

Examples:
  Bad:    let state' = ...
  Better: let next_state = ...
  Better: let state2 = ...
|}

let contains_prime text =
  String.exists (fun ch -> ch = '\'') text

let replacement_for text =
  if String.equal text "" then
    text
  else if String.ends_with ~suffix:"'" text then
    String.sub text 0 (String.length text - 1) ^ "2"
  else
    String.map (fun ch -> if ch = '\'' then '2' else ch) text

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = replacement_for original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostic_for_binding binding =
  if Syn.Cst.LetBinding.is_function binding then
    None
  else
    let name = Syn.Cst.LetBinding.name binding in
    if contains_prime name then
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
