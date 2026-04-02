open Std
open Std.Collections

let rule_id = "snake-case-type-names"

let rule_description = "Type names should use snake_case instead of camelCase"

let rule_explain = {|
Riot treats type names as lower-case names in the same broad family as values and
record fields. `snake_case` keeps signatures visually calm and makes type declarations
look like they belong to the same language as the rest of the code.

Lower-case camelCase names such as `userProfile` are readable, but they introduce an
extra style distinction that readers have to notice and normalize every time they scan
an interface.

Keeping type names boring and predictable is the point. `user_profile` fades into the
background so the meaning of the type can take center stage.
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

let should_flag_type_name = fun text -> not (String.equal text (to_snake_case text))

let make_fix = fun token replacement ->
  Fix.make
    ~title:("Rename type " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " to " ^ replacement)
    ~operations:[ Fix.replace_token_with_text ~target:token ~text:replacement; ]

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement)
    ()

let diagnostic_for_decl type_declaration =
  match type_declaration with
  | Syn.Cst.TypeDeclaration.{ type_name; _ } as decl -> (
      match Syn.Cst.Ident.name type_name with
      | Some text when should_flag_type_name text ->
          let token = Syn.Cst.TypeDeclaration.name_token decl |> Syn.Cst.Token.syntax_token in
          Some (make_diagnostic token)
      | _ -> None
    )

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostic_for_decl decl
          | _ -> None
        )
  | Syn.Cst.Interface { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostic_for_decl decl
          | _ -> None
        )

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_items source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
