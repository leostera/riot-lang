open Std

let rule_id = "avoid-single-letter-type-names"
let rule_description =
  "Type names should be descriptive instead of using single-letter placeholders, except for t"

let rule_explain =
  {|
Single-letter type names hide the shape of an abstraction at exactly the place where
readers most need help. In a signature, `type x` does not tell you whether `x` is a
user profile, a parser state, or a cache entry.

The conventional exception is `t`. When a module has one obvious primary type,
`User.t` or `Cache.t` is the standard OCaml spelling and reads naturally at call sites.

Outside that convention, prefer names that carry domain meaning, such as
`user_profile`, `parser_state`, or `connection_error`.
|}

let should_flag_type_name name =
  String.length name = 1 && not (String.equal name "t")

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
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
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
