open Std
open Std.Collections

let rule_id = Rule_id.of_string "class-case-constructors"

let rule_description = "Variant constructors should use ClassCase instead of underscores"

let rule_explain = {|
Variant constructors read like named cases in a sum type, not like values.
Using `ClassCase` keeps them visually aligned with modules and makes it easy to
distinguish constructors from ordinary bindings.

Mixed styles inside the same type make pattern matches harder to scan. A variant like
`GuestUser | registered_user` forces the reader to switch naming conventions while
reading the same type declaration.

Keep constructors boring and consistent: `GuestUser`, `RegisteredUser`, `Closed`,
`WaitingForInput`.
|}

let contains_underscore = fun text -> String.exists ~fn:(fun ch -> ch = '_') text

let starts_upper = fun text ->
  String.length text > 0 && let ch = String.get_unchecked text ~at:0 in
  ch >= 'A' && ch <= 'Z'

let capitalize_piece = fun piece ->
  if String.equal piece "" then
    ""
  else
    let first = String.make ~len:1 ~char:(String.get_unchecked piece ~at:0 |> Char.uppercase_ascii) in
    let rest =
      if String.length piece = 1 then
        ""
      else
        String.sub piece ~offset:1 ~len:(String.length piece - 1)
    in
    first ^ rest

let to_class_case = fun text ->
  text
  |> String.split ~by:"_"
  |> List.filter ~fn:(fun piece -> not (String.equal piece ""))
  |> List.map ~fn:capitalize_piece
  |> String.concat ""

let should_flag_constructor_name = fun text -> contains_underscore text || not (starts_upper text)

let make_fix = fun token replacement ->
  Fix.make
    ~title:("Rename constructor " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " to " ^ replacement)
    ~operations:[ Fix.replace_token_with_text ~target:token ~text:replacement; ]

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_class_case original in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement)
    ()

let diagnostics_for_decl type_declaration =
  match type_declaration with
  | Syn.Cst.TypeDeclaration.{ type_definition=Syn.Cst.TypeDefinition.Variant { constructors; _ }; _ } ->
      constructors |> List.filter_map
        ~fn:(fun constructor ->
          let name = Syn.Cst.VariantConstructor.name constructor in
          if should_flag_constructor_name name then
            let token = Syn.Cst.VariantConstructor.constructor_name_token constructor
            |> Syn.Cst.Token.syntax_token in
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
