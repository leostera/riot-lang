open Std

let rule_id = "avoid-single-letter-type-names"
let rule_name = "Avoid Single-Letter Type Names"
let rule_code = "F0118"

let rule_description =
  "Type names should be descriptive instead of using single-letter placeholders, except for t"

let rule_message =
  "Type names should be descriptive instead of using single-letter placeholders, except for t."

let rule_explain =
  {|
Single-letter type names should be avoided, except for the conventional `t`.

Why this rule exists:
- Standalone single-letter type names hide the meaning of the abstraction.
- `t` is the one common exception because it is the standard primary type name for a module.

Examples:
  Bad:    type x = ...
  Better: type user_profile = ...
  Good:   type t = ...
|}

let should_flag_type_name name =
  String.length name = 1 && not (String.equal name "t")

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to a descriptive type name")
    ()

let diagnostic_for_decl decl =
  match Syn.Cst.Ident.name (Syn.Cst.TypeDeclaration.type_name decl) with
  | Some name when should_flag_type_name name ->
      let token =
        Syn.Cst.TypeDeclaration.name_token decl
        |> Syn.Cst.Token.syntax_token
      in
      Some (make_diagnostic token)
  | Some _ | None -> None

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostic_for_decl decl
           | _ -> None)
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostic_for_decl decl
           | _ -> None)

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file -> diagnostics_for_items source_file

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
