open Std
open Std.Collections

let rule_id = "snake-case-polyvariant-tags"
let rule_description =
  "Polymorphic variant tags should use snake_case instead of camelCase"

let rule_explain =
  {|
Polymorphic variant tags should use snake_case.

Why this rule exists:
- Riot prefers polyvariant tags that read like values, not constructors.
- snake_case tags are easier to scan inside inline polymorphic variant types.

Examples:
  Bad:    type user = [ `GuestUser | `registered_user ]
  Better: type user = [ `guest_user | `registered_user ]
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

let should_flag_tag_name text =
  not (String.equal text (to_snake_case text))

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_snake_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostics_for_decl = function
  | Syn.Cst.TypeDeclaration.{ type_definition = Syn.Cst.TypeDefinition.PolyVariant poly_variant; _ } ->
      Syn.Cst.PolyVariant.tags poly_variant
      |> List.filter_map (fun tag ->
             let name = Syn.Cst.PolyVariantTag.name tag in
             if should_flag_tag_name name then
               let token =
                 Syn.Cst.PolyVariantTag.tag_name_token tag
                 |> Syn.Cst.Token.syntax_token
               in
               Some (make_diagnostic token)
             else
               None)
  | _ -> []

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostics_for_decl decl
           | _ -> [])
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostics_for_decl decl
           | _ -> [])

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file -> diagnostics_for_items source_file

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
