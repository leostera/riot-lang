open Std
open Std.Collections

let rule_id = "prefer-opaque-record-types"
let rule_description =
  "Interfaces that already expose accessor functions should usually keep record types opaque"

let rule_explain =
  {|
Prefer opaque record types once the interface already exposes accessor functions.

Examples:
  Avoid:   type t = { name : string }
           val name : t -> string

  Better:  type t
           val name : t -> string

If callers already go through functions like `name`, exposing the record fields
directly usually just makes the representation harder to change later.
|}

let rec strip_type_wrappers = function
  | Syn.Cst.CoreType.Parenthesized { inner; _ }
  | Syn.Cst.CoreType.Attribute { type_ = inner; _ }
  | Syn.Cst.CoreType.Alias { type_ = inner; _ } ->
      strip_type_wrappers inner
  | type_ ->
      type_

let is_t_core_type type_ =
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Constr { constructor_path; arguments = []; _ } -> (
      match Syn.Cst.Ident.name constructor_path with
      | Some "t" ->
          true
      | _ ->
          false)
  | _ ->
      false

let rec first_arrow_parameter_type type_ =
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Arrow { parameter_type; _ } ->
      Some parameter_type
  | Syn.Cst.CoreType.Poly { body; _ } ->
      first_arrow_parameter_type body
  | _ ->
      None

let is_accessor_of_t ({ type_; _ } : Syn.Cst.value_declaration) =
  match first_arrow_parameter_type type_ with
  | Some parameter_type ->
      is_t_core_type parameter_type
  | None ->
      false

let make_diagnostic (decl : Syn.Cst.TypeDeclaration.t) accessor_names =
  let accessor_summary =
    match accessor_names with
    | [] ->
        "exposed accessors"
    | [ name ] ->
        "the `" ^ name ^ "` accessor"
    | first :: second :: _ ->
        "accessors like `" ^ first ^ "` and `" ^ second ^ "`"
  in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.TypeDeclaration.syntax_node decl))
    ~suggestion:
      ("Make `t` opaque in the interface and keep "
     ^ accessor_summary ^ " as the public surface")
    ()

let diagnostic_for_type_declaration value_declarations decl =
  match Syn.Cst.TypeDeclaration.type_name decl |> Syn.Cst.Ident.name with
  | Some "t" -> (
      match Syn.Cst.TypeDeclaration.type_definition decl with
      | Syn.Cst.TypeDefinition.Record { fields; _ } ->
          let field_names =
            fields |> List.map Syn.Cst.RecordField.name
          in
          let accessor_names =
            value_declarations
            |> List.filter is_accessor_of_t
            |> List.map (fun (value_decl : Syn.Cst.value_declaration) ->
                   Syn.Cst.Token.text value_decl.name_token)
            |> List.filter (fun name -> List.mem name field_names)
          in
          if List.length accessor_names > 0 then
            Some (make_diagnostic decl accessor_names)
          else
            None
      | _ ->
          None)
  | _ ->
      None

let check_tree (ctx : Rule.context) _red_root =
  if not (String.ends_with ~suffix:".mli" ctx.file_path) then
    []
  else
    match ctx.cst with
    | None ->
        []
    | Some source_file -> (
        match Syn.Cst.SourceFile.signature_items source_file with
        | None ->
            []
        | Some items ->
            let value_declarations =
              items
              |> List.filter_map (function
                   | Syn.Cst.SignatureItem.ValueDeclaration decl ->
                       Some decl
                   | _ ->
                       None)
            in
            items
            |> List.filter_map (function
                 | Syn.Cst.SignatureItem.TypeDeclaration decl ->
                     diagnostic_for_type_declaration value_declarations decl
                 | _ ->
                     None))

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
