open Std

let rule_id = "no-public-mutable-fields"

let rule_description = "Interface types should not expose mutable record fields"

let rule_explain = {|
Publishing a mutable field in an interface means every caller can reach in and change
that piece of state directly. Once that happens, the implementation can no longer
control invariants around the update, and readers have a harder time reasoning about
who is allowed to mutate what.

Keeping the representation opaque gives the module room to enforce invariants and to
change its internal layout later. If callers need mutation, expose operations such as
`set_state`, `replace`, or `reset` instead of exposing the field itself.

The point is not to forbid mutation. It is to keep ownership of mutation inside the
module that understands the invariants.
|}

let make_diagnostic = fun (field: Syn.Cst.RecordField.t) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span field.syntax_node)
    ~suggestion:"Keep this mutable field private to the implementation and expose operations instead."
    ()

let diagnostics_for_decl = fun ({ type_definition; _ }: Syn.Cst.TypeDeclaration.t) ->
  match type_definition with
  | Syn.Cst.TypeDefinition.Record { fields; _ } ->
      fields |> List.filter_map ~fn:(fun (field: Syn.Cst.RecordField.t) ->
          if Option.is_some field.mutable_token then
            Some (make_diagnostic field)
          else
            None)
  | _ -> []

let check_tree = fun (ctx: Rule.context) _red_root ->
  if not (String.ends_with ~suffix:".mli" ctx.file_path) then
    []
  else
    let source_file = ctx.cst in
    (
      match Syn.Cst.SourceFile.signature_items source_file with
      | Some items ->
          items
          |> List.map ~fn:(
              function
              | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostics_for_decl decl
              | _ -> []
            )
          |> List.concat
      | None -> []
    )

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
