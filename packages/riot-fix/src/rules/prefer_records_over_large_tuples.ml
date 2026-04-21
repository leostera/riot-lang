open Std
open Std.Collections

let rule_id = Rule_id.of_string "prefer-records-over-large-tuples"

let rule_description = "Large tuple type aliases should usually be records"

let rule_explain = {|
Small tuples work well for compact, obviously positional data such as pairs and simple
triples. Once the tuple gets large, or once several elements have the same type, the
reader has to remember an implicit field order that the type does not name.

At that point a record usually communicates the data model better. Field names make it
clear what each position means, and they give the type room to evolve without forcing
every caller to memorize or reshuffle tuple order.

If a type like `string * string * string * string` needs a comment to explain which
slot is which, it has already crossed the line where a record is a better fit.
|}

let simple_type_name = function
  | Syn.Cst.CoreType.Constr { constructor_path; arguments=[]; _ } -> Syn.Cst.Ident.name constructor_path
  | Syn.Cst.CoreType.Var { name_token; _ } -> Some (Syn.Cst.Token.text name_token)
  | _ -> None

let should_prefer_record = fun elements ->
  let count = List.length elements in
  if count > 4 then
    true
  else if count <= 3 then
    false
  else
    match elements with
    | [] -> false
    | first :: rest -> (
        match simple_type_name first with
        | None -> false
        | Some first_name ->
            List.all rest
              ~fn:(fun element ->
                match simple_type_name element with
                | Some name -> String.equal name first_name
                | None -> false)
      )

let make_diagnostic = fun (decl: Syn.Cst.TypeDeclaration.t) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.TypeDeclaration.syntax_node decl))
    ~suggestion:"Replace this tuple alias with a record type so each position has a stable field name."
    ()

let make_type_diagnostic = fun syntax_node ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Replace this tuple type with a record type so each position has a stable field name."
    ()

let rec diagnostics_for_core_type = fun type_ ->
  match type_ with
  | Syn.Cst.CoreType.Wildcard _
  | Syn.Cst.CoreType.Var _
  | Syn.Cst.CoreType.Extension _ ->
      []
  | Syn.Cst.CoreType.Constr { arguments; _ }
  | Syn.Cst.CoreType.Class { arguments; _ } ->
      arguments |> List.map ~fn:diagnostics_for_core_type |> List.concat
  | Syn.Cst.CoreType.Alias { type_; _ }
  | Syn.Cst.CoreType.Attribute { type_; _ }
  | Syn.Cst.CoreType.Parenthesized { inner=type_; _ } ->
      diagnostics_for_core_type type_
  | Syn.Cst.CoreType.Poly { body; _ } ->
      diagnostics_for_core_type body
  | Syn.Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      diagnostics_for_core_type parameter_type @ diagnostics_for_core_type result_type
  | Syn.Cst.CoreType.Tuple { syntax_node; elements } ->
      let here =
        if should_prefer_record elements then
          [ make_type_diagnostic syntax_node ]
        else
          []
      in
      here @ (elements |> List.map ~fn:diagnostics_for_core_type |> List.concat)
  | Syn.Cst.CoreType.PolyVariant { fields; _ } ->
      fields |> List.map
        ~fn:(
          function
          | Syn.Cst.RowField.Tag { payload_type; _ } -> Option.to_list payload_type
          |> List.map ~fn:diagnostics_for_core_type
          |> List.concat
          | Syn.Cst.RowField.Inherit { type_; _ } -> diagnostics_for_core_type type_
        ) |> List.concat
  | Syn.Cst.CoreType.Record { fields; _ } ->
      fields
      |> List.map
        ~fn:(fun (field: Syn.Cst.record_type_field) -> diagnostics_for_core_type field.field_type)
      |> List.concat
  | Syn.Cst.CoreType.FirstClassModule _ ->
      []
  | Syn.Cst.CoreType.Object { fields; _ } ->
      fields
      |> List.map
        ~fn:(fun (field: Syn.Cst.object_type_field) -> diagnostics_for_core_type field.field_type)
      |> List.concat

let diagnostics_for_variant_constructor = fun (constructor: Syn.Cst.VariantConstructor.t) ->
  let from_arguments =
    match Syn.Cst.VariantConstructor.arguments constructor with
    | Some (Syn.Cst.ConstructorArguments.Tuple types) -> types
    |> List.map ~fn:diagnostics_for_core_type
    |> List.concat
    | Some (Syn.Cst.ConstructorArguments.Record { fields; _ }) -> fields
    |> List.map
      ~fn:(fun (field: Syn.Cst.RecordField.t) -> diagnostics_for_core_type field.field_type)
    |> List.concat
    | None -> []
  in
  from_arguments
  @ (Syn.Cst.VariantConstructor.payload_type constructor
  |> Option.to_list
  |> List.map ~fn:diagnostics_for_core_type
  |> List.concat)
  @ (Syn.Cst.VariantConstructor.result_type constructor
  |> Option.to_list
  |> List.map ~fn:diagnostics_for_core_type
  |> List.concat)

let diagnostics_for_type_definition = function
  | Syn.Cst.TypeDefinition.Abstract
  | Syn.Cst.TypeDefinition.Extensible _ -> []
  | Syn.Cst.TypeDefinition.PolyVariant _ -> []
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } -> diagnostics_for_core_type manifest
  | Syn.Cst.TypeDefinition.FirstClassModule _ -> []
  | Syn.Cst.TypeDefinition.Object { fields; _ } -> fields
  |> List.map
    ~fn:(fun (field: Syn.Cst.object_type_field) -> diagnostics_for_core_type field.field_type)
  |> List.concat
  | Syn.Cst.TypeDefinition.Record { fields; _ } -> fields
  |> List.map ~fn:(fun (field: Syn.Cst.RecordField.t) -> diagnostics_for_core_type field.field_type)
  |> List.concat
  | Syn.Cst.TypeDefinition.Variant { constructors; _ } -> constructors
  |> List.map ~fn:diagnostics_for_variant_constructor
  |> List.concat

let diagnostics_for_type_declaration = fun decl ->
  let from_definition =
    match Syn.Cst.TypeDeclaration.type_definition decl with
    | Syn.Cst.TypeDefinition.Alias { manifest=Syn.Cst.CoreType.Tuple { elements; _ }; _ } when should_prefer_record
      elements -> [ make_diagnostic decl ]
    | definition -> diagnostics_for_type_definition definition
  in
  from_definition
  @ ((Syn.Cst.TypeDeclaration.constraints decl
  |> List.map
    ~fn:(fun (constraint_: Syn.Cst.TypeConstraint.t) ->
      diagnostics_for_core_type constraint_.left @ diagnostics_for_core_type constraint_.right))
  |> List.concat)

let diagnostics_for_value_declaration = fun ({ type_; _ }: Syn.Cst.value_declaration) ->
  diagnostics_for_core_type type_

let diagnostics_for_external_declaration = fun ({ type_; _ }: Syn.Cst.external_declaration) ->
  diagnostics_for_core_type type_

let diagnostics_for_source_file = function
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.map
        ~fn:(
          function
          | Syn.Cst.StructureItem.TypeDeclaration decl -> diagnostics_for_type_declaration decl
          | Syn.Cst.StructureItem.ExternalDeclaration decl -> diagnostics_for_external_declaration decl
          | _ -> []
        ) |> List.concat
  | Syn.Cst.Interface { items; _ } ->
      items |> List.map
        ~fn:(
          function
          | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostics_for_type_declaration decl
          | Syn.Cst.SignatureItem.ValueDeclaration decl -> diagnostics_for_value_declaration decl
          | _ -> []
        ) |> List.concat

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_source_file source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
