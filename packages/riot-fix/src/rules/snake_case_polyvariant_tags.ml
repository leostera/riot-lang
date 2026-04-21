open Std
open Std.Collections

let rule_id = Rule_id.of_string "snake-case-polyvariant-tags"

let rule_description = "Polymorphic variant tags should use snake_case instead of camelCase"

let rule_explain = {|
Riot treats polymorphic variant tags more like value-level names than constructor-level
names, so `snake_case` keeps them aligned with the rest of the language.

That choice matters most in inline polymorphic variant types, where the tags appear in
dense type expressions and should stay easy to scan. Mixing `GuestUser` and
`registered_user` in the same row makes the type feel visually inconsistent for no real
benefit.

Use `snake_case` tags such as `` `guest_user `` and `` `registered_user `` so the style
stays predictable across the type surface.
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
  String.for_each text
    ~fn:(fun ch ->
      if is_upper ch then
        (
          if !prev_was_lower_or_digit then
            push "_";
          push (String.make ~len:1 ~char:(Char.lowercase_ascii ch));
          prev_was_lower_or_digit := false
        )
      else (
        push (String.make ~len:1 ~char:ch);
        prev_was_lower_or_digit := is_lower ch || is_digit ch
      ));
  String.concat "" (List.reverse !pieces)

let should_flag_tag_name = fun text -> not (String.equal text (to_snake_case text))

let make_fix = fun token replacement ->
  Fix.make
    ~title:("Rename polyvariant tag " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " to " ^ replacement)
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

let diagnostics_for_decl type_declaration =
  match type_declaration with
  | Syn.Cst.TypeDeclaration.{ type_definition=Syn.Cst.TypeDefinition.PolyVariant poly_variant; _ } ->
      Syn.Cst.PolyVariant.tags poly_variant |> List.filter_map
        ~fn:(fun tag ->
          let name = Syn.Cst.PolyVariantTag.name tag in
          if should_flag_tag_name name then
            let token = Syn.Cst.PolyVariantTag.tag_name_token tag |> Syn.Cst.Token.syntax_token in
            Some (make_diagnostic token)
          else
            None)
  | _ -> []

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.map
        ~fn:(
          function
          | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostics_for_decl decl
          | _ -> []
        ) |> List.concat
  | Syn.Cst.Interface { items; _ } ->
      items |> List.map
        ~fn:(
          function
          | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostics_for_decl decl
          | _ -> []
        ) |> List.concat

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_items source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
