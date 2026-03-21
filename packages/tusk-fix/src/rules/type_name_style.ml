open Std
open Std.Collections

let rule_id = "snake-case-type-names"
let rule_name = "Snake Case Type Names"

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

let diagnostic_for_decl = function
  | Syn.Cst.TypeDeclaration.{ type_name; _ } as decl -> (
      match Syn.Cst.ModulePath.name type_name with
      | Some text when should_flag_type_name text ->
          let token =
            Syn.Cst.TypeDeclaration.name_token decl
            |> Syn.Cst.Token.syntax_token
          in
          Some (make_diagnostic token)
      | _ -> None)

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.items source_file
      |> List.filter_map (function
           | Syn.Cst.Item.TypeDeclaration decl -> diagnostic_for_decl decl
           | Syn.Cst.Item.Unknown _ -> None)

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
