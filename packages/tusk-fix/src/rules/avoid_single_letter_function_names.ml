open Std

let rule_id = "avoid-single-letter-function-names"
let rule_name = "Avoid Single-Letter Function Names"
let rule_code = "F0117"

let rule_description =
  "Function names should be descriptive instead of using single-letter placeholders"

let rule_message =
  "Function names should be descriptive instead of using single-letter placeholders."

let rule_explain =
  {|
Single-letter function names like `f` or `g` should be avoided.

Why this rule exists:
- Placeholder names make call sites and stack traces harder to read.
- Most functions end up carrying domain meaning that should be reflected in the binding name.

Examples:
  Bad:    let f user = ...
  Better: let parse_user user = ...
|}

let should_flag_function_name name =
  String.length name = 1

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to a descriptive function name")
    ()

let diagnostic_for_binding binding =
  if not (Syn.Cst.LetBinding.is_function binding) then
    None
  else
    let name = Syn.Cst.LetBinding.name binding in
    if should_flag_function_name name then
      match Syn.Cst.LetBinding.binding_name_token binding with
      | Some token ->
          Some (make_diagnostic (Syn.Cst.Token.syntax_token token))
      | None -> None
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
