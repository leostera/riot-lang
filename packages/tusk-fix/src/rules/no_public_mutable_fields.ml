open Std

let rule_id = "no-public-mutable-fields"
let rule_name = "No Public Mutable Fields"
let rule_code = "F0141"

let rule_description =
  "Interface types should not expose mutable record fields"

let rule_message =
  "Do not expose mutable record fields in public interfaces."

let rule_explain =
  {|
Avoid public mutable record fields in `.mli` files.

Once a mutable field appears in an interface, any caller can update it behind your back.
That makes local reasoning harder because the value can change from any code that holds the record.
Keep mutation private inside the implementation and expose operations instead.

Examples:
  Avoid:   type t = { mutable state : state }
  Better:  type t
           val set_state : t -> state -> unit
|}

let make_diagnostic (field : Syn.Cst.RecordField.t) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span field.syntax_node)
    ~suggestion:"Keep this mutable field private to the implementation and expose operations instead."
    ()

let diagnostics_for_decl ({ type_definition; _ } : Syn.Cst.TypeDeclaration.t) =
  match type_definition with
  | Syn.Cst.TypeDefinition.Record fields ->
      fields
      |> List.filter_map (fun (field : Syn.Cst.RecordField.t) ->
             if field.is_mutable then
               Some (make_diagnostic field)
             else
               None)
  | _ -> []

let check_tree (ctx : Rule.context) _red_root =
  if not (String.ends_with ~suffix:".mli" ctx.file_path) then
    []
  else
    match ctx.cst with
    | None -> []
    | Some source_file ->
        Syn.Cst.SourceFile.items source_file
        |> List.concat_map (function
             | Syn.Cst.Item.TypeDeclaration decl -> diagnostics_for_decl decl
             | _ -> [])

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
