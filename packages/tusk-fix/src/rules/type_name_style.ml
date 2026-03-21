open Std
open Std.Collections

let rule_id = "type-name-style"
let rule_name = "Type Name Style"

let rule_description =
  "Type names should use snake_case instead of camelCase"

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

let should_flag_type_name text =
  not (String.equal text (to_snake_case text))

let make_fix token replacement =
  Fix.make
    ~title:
      ("Rename type " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " to "
     ^ replacement)
    ~edits:
      [
        Fix.make_text_edit ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
          ~new_text:replacement;
      ]

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known Diagnostic_code.CamelCaseTypeName)
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement) ()

let type_name_token node =
  let children = Syn.Ceibo.Red.SyntaxNode.children node in
  let last_non_trivia_token node =
    let module_path_children = Syn.Ceibo.Red.SyntaxNode.children node in
    let rec find idx =
      if idx < 0 then None
      else
        match module_path_children.(idx) with
        | Syn.Ceibo.Red.Token token
          when not (Traversal.is_trivia (Syn.Ceibo.Red.SyntaxToken.kind token)) ->
            Some token
        | _ -> find (idx - 1)
    in
    find (Array.length module_path_children - 1)
  in
  let rec find idx =
    if idx >= Array.length children then None
    else
      match children.(idx) with
      | Syn.Ceibo.Red.Node child
        when Syn.Ceibo.Red.SyntaxNode.kind child = Syn.SyntaxKind.MODULE_PATH ->
          last_non_trivia_token child
      | _ -> find (idx + 1)
  in
  find 0

let diagnostic_for_decl node =
  match type_name_token node with
  | Some token ->
      let text = Syn.Ceibo.Red.SyntaxToken.text token in
      if should_flag_type_name text then Some (make_diagnostic token)
      else None
  | None -> None

let check_tree _ctx red_root =
  Traversal.find_by_kind Syn.SyntaxKind.TYPE_DECL red_root
  |> List.filter_map diagnostic_for_decl

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
