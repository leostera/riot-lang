open Std
open Std.Collections

let rule_id = "prefer-named-closed-polyvariants"
let rule_description =
  "Closed polymorphic variants should usually be named instead of written inline"

let rule_explain =
  {|
Inline closed polymorphic variants are convenient in tiny signatures, but once the same
shape starts appearing in several places they become anonymous protocol fragments that
every reader has to rediscover.

Giving the closed variant a name turns it into a real part of the API. `type format =
[ \`json | \`xml ]` is easier to reuse, easier to document, and easier to evolve than
repeating `[ \`json | \`xml ]` throughout values and type aliases.

If the closed set of tags matters enough to appear in public types, it usually matters
enough to deserve a proper name.
|}

let make_diagnostic syntax_node =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:
      "Introduce a named type alias for this closed polymorphic variant and use that name instead"
    ()

let rec diagnostics_for_core_type type_ =
  match type_ with
  | Syn.Cst.CoreType.Wildcard _
  | Syn.Cst.CoreType.Var _
  | Syn.Cst.CoreType.Extension _ ->
      []
  | Syn.Cst.CoreType.Constr { arguments; _ }
  | Syn.Cst.CoreType.Class { arguments; _ } ->
      arguments |> List.concat_map diagnostics_for_core_type
  | Syn.Cst.CoreType.Alias { type_; _ }
  | Syn.Cst.CoreType.Attribute { type_; _ }
  | Syn.Cst.CoreType.Parenthesized { inner = type_; _ } ->
      diagnostics_for_core_type type_
  | Syn.Cst.CoreType.Poly { body; _ } ->
      diagnostics_for_core_type body
  | Syn.Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      diagnostics_for_core_type parameter_type
      @ diagnostics_for_core_type result_type
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      elements |> List.concat_map diagnostics_for_core_type
  | Syn.Cst.CoreType.PolyVariant { syntax_node; kind; fields } ->
      let here =
        match kind with
        | Syn.Cst.PolyVariantBound.Exact ->
            [ make_diagnostic syntax_node ]
        | Syn.Cst.PolyVariantBound.UpperBound _
        | Syn.Cst.PolyVariantBound.LowerBound _ ->
            []
      in
      let nested =
        fields
        |> List.concat_map (function
             | Syn.Cst.RowField.Tag { payload_type; _ } ->
                 Option.to_list payload_type
                 |> List.concat_map diagnostics_for_core_type
             | Syn.Cst.RowField.Inherit { type_; _ } ->
                 diagnostics_for_core_type type_)
      in
      here @ nested
  | Syn.Cst.CoreType.Record { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.record_type_field) ->
             diagnostics_for_core_type field.field_type)
  | Syn.Cst.CoreType.FirstClassModule _ ->
      []
  | Syn.Cst.CoreType.Object { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.object_type_field) ->
             diagnostics_for_core_type field.field_type)

let diagnostics_for_variant_constructor
    (constructor : Syn.Cst.VariantConstructor.t) =
  let from_arguments =
    match Syn.Cst.VariantConstructor.arguments constructor with
    | Some (Syn.Cst.ConstructorArguments.Tuple types) ->
        types |> List.concat_map diagnostics_for_core_type
    | Some (Syn.Cst.ConstructorArguments.Record { fields; _ }) ->
        fields
        |> List.concat_map (fun (field : Syn.Cst.RecordField.t) ->
               diagnostics_for_core_type field.field_type)
    | None ->
        []
  in
  from_arguments
  @
  (Syn.Cst.VariantConstructor.payload_type constructor
  |> Option.to_list
  |> List.concat_map diagnostics_for_core_type)
  @
  (Syn.Cst.VariantConstructor.result_type constructor
  |> Option.to_list
  |> List.concat_map diagnostics_for_core_type)

let diagnostics_for_type_definition = function
  | Syn.Cst.TypeDefinition.Abstract
  | Syn.Cst.TypeDefinition.Extensible _ ->
      []
  | Syn.Cst.TypeDefinition.PolyVariant _ ->
      []
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      diagnostics_for_core_type manifest
  | Syn.Cst.TypeDefinition.FirstClassModule _ ->
      []
  | Syn.Cst.TypeDefinition.Object { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.object_type_field) ->
             diagnostics_for_core_type field.field_type)
  | Syn.Cst.TypeDefinition.Record { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.RecordField.t) ->
             diagnostics_for_core_type field.field_type)
  | Syn.Cst.TypeDefinition.Variant { constructors; _ } ->
      constructors |> List.concat_map diagnostics_for_variant_constructor

let diagnostics_for_type_declaration decl =
  diagnostics_for_type_definition (Syn.Cst.TypeDeclaration.type_definition decl)
  @
  (Syn.Cst.TypeDeclaration.constraints decl
  |> List.concat_map (fun (constraint_ : Syn.Cst.TypeConstraint.t) ->
         diagnostics_for_core_type constraint_.left
         @ diagnostics_for_core_type constraint_.right))

let diagnostics_for_value_declaration
    ({ type_; _ } : Syn.Cst.value_declaration) =
  diagnostics_for_core_type type_

let diagnostics_for_external_declaration
    ({ type_; _ } : Syn.Cst.external_declaration) =
  diagnostics_for_core_type type_

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.StructureItem.TypeDeclaration decl ->
               diagnostics_for_type_declaration decl
           | Syn.Cst.StructureItem.ExternalDeclaration decl ->
               diagnostics_for_external_declaration decl
           | _ ->
               [])
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.SignatureItem.TypeDeclaration decl ->
               diagnostics_for_type_declaration decl
           | Syn.Cst.SignatureItem.ValueDeclaration decl ->
               diagnostics_for_value_declaration decl
           | _ ->
               [])

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:(fun ctx _red_root ->
      let source_file = ctx.cst in diagnostics_for_items source_file)
    ()
