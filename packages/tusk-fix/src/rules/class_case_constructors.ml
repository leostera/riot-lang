open Std
open Std.Collections

let rule_id = "class-case-constructors"
let rule_name = "Class Case Constructors"
let rule_code = "F0115"

let rule_description =
  "Variant constructors should use ClassCase instead of underscores"

let rule_message =
  "Variant constructors should use ClassCase without underscores."

let rule_explain =
  {|
Variant constructors should use ClassCase without underscores.

Why this rule exists:
- Constructors read like named cases, so they should visually line up with modules rather than values.
- Mixed styles like guest_user make sum types harder to scan.

Examples:
  Bad:    type user = | guest_user | RegisteredUser
  Better: type user = | GuestUser | RegisteredUser
|}

let contains_underscore text =
  String.exists (fun ch -> ch = '_') text

let starts_upper text =
  String.length text > 0
  && let ch = String.get text 0 in
     ch >= 'A' && ch <= 'Z'

let capitalize_piece piece =
  if String.equal piece "" then
    ""
  else
    let first = String.get piece 0 |> Char.uppercase_ascii |> String.make 1 in
    let rest =
      if String.length piece = 1 then
        ""
      else
        String.sub piece 1 (String.length piece - 1)
    in
    first ^ rest

let to_class_case text =
  text
  |> String.split_on_char '_'
  |> List.filter (fun piece -> not (String.equal piece ""))
  |> List.map capitalize_piece
  |> String.concat ""

let should_flag_constructor_name text =
  contains_underscore text || not (starts_upper text)

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_class_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostics_for_decl = function
  | Syn.Cst.TypeDeclaration.{ type_definition = Syn.Cst.TypeDefinition.Variant constructors; _ } ->
      constructors
      |> List.filter_map (fun constructor ->
             let name = Syn.Cst.VariantConstructor.name constructor in
             if should_flag_constructor_name name then
               let token =
                 Syn.Cst.VariantConstructor.constructor_name_token constructor
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
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
