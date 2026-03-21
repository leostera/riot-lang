open Std

let rule_id = "no-unnecessary-rec"
let rule_name = "No Unnecessary Rec"
let rule_code = "F0127"

let rule_description =
  "Recursive bindings should only use rec when they actually self-reference"

let rule_message =
  "Remove rec from bindings that do not reference themselves."

let rule_explain =
  {|
Recursive bindings should only use rec when they actually self-reference.

Why this rule exists:
- rec signals recursion and makes the reader look for a recursive call.
- When the binding never refers to itself, rec adds noise without changing behavior.
- Removing unnecessary rec makes the real recursive definitions stand out more clearly.

Examples:
  Bad:    let rec render x = x + 1
  Better: let render x = x + 1

When this warning fires, Remove rec from the binding.
|}

let self_references_binding binding =
  let name = Syn.Cst.LetBinding.name binding in
  let value_node = Syn.Cst.LetBinding.value_syntax_node binding in
  Traversal.find_tokens
    (fun token -> String.equal (Syn.Ceibo.Red.SyntaxToken.text token) name)
    value_node
  |> List.length
  |> fun count -> count > 0

let rec_token binding =
  Traversal.find_tokens
    (fun token -> String.equal (Syn.Ceibo.Red.SyntaxToken.text token) "rec")
    (Syn.Cst.LetBinding.syntax_node binding)
  |> function
  | token :: _ -> Some token
  | [] -> None

let make_diagnostic binding =
  let span =
    match rec_token binding with
    | Some token -> Syn.Ceibo.Red.SyntaxToken.span token
    | None -> Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.LetBinding.syntax_node binding)
  in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span
    ~suggestion:"Remove rec from this binding."
    ()

let diagnostic_for_binding binding =
  if Syn.Cst.LetBinding.is_recursive binding && not (self_references_binding binding) then
    Some (make_diagnostic binding)
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
