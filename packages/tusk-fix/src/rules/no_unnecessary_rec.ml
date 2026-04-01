open Std

let rule_id = "no-unnecessary-rec"

let rule_description = "Recursive bindings should only use rec when they actually self-reference"

let rule_explain = {|
`rec` is a signal to the reader that the definition is recursive. Once that keyword is
present, people quite reasonably start looking for the recursive call and for the base
case that keeps it safe.

If the binding never refers to itself, that signal is false. The keyword adds noise,
and it makes genuinely recursive definitions harder to spot because the eye can no
longer trust `rec` to mean "this one actually loops back."

When this warning fires, the fix is simple: remove `rec` and let recursive bindings
stand out only where recursion is real.
|}

let self_references_binding = fun binding ->
  match Syn.Cst.LetBinding.binding_name_token binding with
  | None ->
      (* Anonymous or pattern bindings cannot be checked for self-reference by name.
         Keep the rule conservative and skip them instead of crashing the run. *)
      true
  | Some name_token ->
      let name = Syn.Cst.Token.text name_token in
      let value_node = Syn.Cst.LetBinding.value_syntax_node binding in
      Traversal.find_tokens
        (fun token ->
          String.equal (Syn.Ceibo.Red.SyntaxToken.text token) name)
        value_node |> List.length |> fun count -> count > 0

let rec_token = fun binding ->
  Traversal.find_tokens
    (fun token ->
      String.equal (Syn.Ceibo.Red.SyntaxToken.text token) "rec")
    (Syn.Cst.LetBinding.syntax_node binding) |> function
  | token :: _ -> Some token
  | [] -> None

let make_diagnostic = fun binding ->
  let span =
    match rec_token binding with
    | Some token -> Syn.Ceibo.Red.SyntaxToken.span token
    | None -> Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.LetBinding.syntax_node binding)
  in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known {rule_id;message = rule_description;})
    ~span
    ~suggestion:"Remove rec from this binding."
    ()

let diagnostic_for_binding = fun binding ->
  if Syn.Cst.LetBinding.is_recursive binding && not (self_references_binding binding) then
    Some (make_diagnostic binding)
  else
    None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.filter_map diagnostic_for_binding

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
