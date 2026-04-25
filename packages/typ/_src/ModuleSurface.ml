open Std
open Model

type qualified_surface = { exports: FileSummary.exports; type_decls: FileSummary.type_decl list }

type local_type_decl_index = {
  by_path: (SurfacePath.t, FileSummary.type_decl) Collections.HashMap.t;
  by_id: (TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t;
}

let type_decl_key = fun (type_decl: FileSummary.type_decl) -> SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name

let local_type_decl_index = fun type_decls ->
  let by_path = Collections.HashMap.with_capacity (List.length type_decls) in
  let by_id = Collections.HashMap.with_capacity (List.length type_decls) in
  List.iter
    (
      fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in ()
    )
    type_decls;
  { by_path; by_id }

let qualify_local_head = fun local_types module_name (head: TypeRepr.named_type_head) ->
  match Collections.HashMap.get local_types.by_id head.type_constructor_id with
  | Some type_decl -> { head with name = SurfacePath.prepend_name module_name (type_decl_key type_decl) }
  | None -> (
    match Collections.HashMap.get local_types.by_path head.name with
    | Some _ -> { head with name = SurfacePath.prepend_name module_name head.name }
    | None -> head
  )

let rec qualify_type = fun local_types module_name ty ->
  let ty = TypeRepr.prune ty in
  match TypeRepr.view ty with
  | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ | TypeRepr.Var _ -> ty
  | TypeRepr.Option element ->
      let qualified_element = qualify_type local_types module_name element in
      if Std.Ptr.equal element qualified_element then
        ty
      else TypeRepr.option qualified_element
  | TypeRepr.Result (ok_ty, error_ty) ->
      let qualified_ok_ty = qualify_type local_types module_name ok_ty in
      let qualified_error_ty = qualify_type local_types module_name error_ty in
      if Std.Ptr.equal ok_ty qualified_ok_ty && Std.Ptr.equal error_ty qualified_error_ty then
        ty
      else TypeRepr.result qualified_ok_ty qualified_error_ty
  | TypeRepr.Array element ->
      let qualified_element = qualify_type local_types module_name element in
      if Std.Ptr.equal element qualified_element then
        ty
      else TypeRepr.array qualified_element
  | TypeRepr.List element ->
      let qualified_element = qualify_type local_types module_name element in
      if Std.Ptr.equal element qualified_element then
        ty
      else TypeRepr.list qualified_element
  | TypeRepr.Seq element ->
      let qualified_element = qualify_type local_types module_name element in
      if Std.Ptr.equal element qualified_element then
        ty
      else TypeRepr.seq qualified_element
  | TypeRepr.Named { head; arguments } ->
      let qualified_arguments = List.map (qualify_type local_types module_name) arguments in
      let qualified_head = qualify_local_head local_types module_name head in
      if Std.Ptr.equal head qualified_head && List.for_all2 Std.Ptr.equal arguments qualified_arguments then
        ty
      else TypeRepr.named ~head:qualified_head ~arguments:qualified_arguments
  | TypeRepr.PolyVariant { bound; tags; inherited } ->
      let qualified_tags =
        tags |> List.map
          (
            fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let qualified_payload_type = qualify_type local_types module_name payload_type in
                  if Std.Ptr.equal payload_type qualified_payload_type then
                    tag
                  else { tag with payload_type = Some qualified_payload_type }
              | None -> tag
          )
      in
      let qualified_inherited = List.map (qualify_type local_types module_name) inherited in
      if List.for_all2 Std.Ptr.equal tags qualified_tags && List.for_all2 Std.Ptr.equal inherited qualified_inherited then
        ty
      else TypeRepr.poly_variant ~bound ~tags:qualified_tags ~inherited:qualified_inherited
  | TypeRepr.Tuple members ->
      let qualified_members = List.map (qualify_type local_types module_name) members in
      if List.for_all2 Std.Ptr.equal members qualified_members then
        ty
      else TypeRepr.tuple qualified_members
  | TypeRepr.Arrow { label; lhs; rhs } ->
      let qualified_lhs = qualify_type local_types module_name lhs in
      let qualified_rhs = qualify_type local_types module_name rhs in
      if Std.Ptr.equal lhs qualified_lhs && Std.Ptr.equal rhs qualified_rhs then
        ty
      else TypeRepr.arrow ~label ~lhs:qualified_lhs ~rhs:qualified_rhs
  | TypeRepr.Package signature ->
      let qualified_values =
        signature.values |> List.map
          (
            fun (value: TypeRepr.package_value) ->
              let qualified_scheme = TypeScheme.map_type_preserving (qualify_type local_types module_name) value.scheme in
              if Std.Ptr.equal value.scheme qualified_scheme then
                value
              else { value with scheme = qualified_scheme }
          )
      in
      if List.for_all2 Std.Ptr.equal signature.values qualified_values then
        ty
      else TypeRepr.package ~values:qualified_values

let qualify_scheme_with_local_types = fun local_types ~module_name scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  let qualified_body = qualify_type local_types module_name body in
  if Std.Ptr.equal body qualified_body then
    scheme
  else TypeScheme.of_explicit ~quantified qualified_body

let qualify_scheme = fun ~type_decls ~module_name scheme ->
  let local_types = local_type_decl_index type_decls in
  let quantified, body = TypeScheme.to_explicit scheme in
  let qualified_body = qualify_type local_types module_name body in
  if Std.Ptr.equal body qualified_body then
    scheme
  else TypeScheme.of_explicit ~quantified qualified_body

let qualify_inline_record_labels = fun local_types module_name labels ->
  labels |> List.map
    (
      fun (label: TypeDecl.label) ->
        let qualified_field_type = TypeScheme.map_type_preserving (qualify_type local_types module_name) label.field_type in
        if Std.Ptr.equal label.field_type qualified_field_type then
          label
        else { label with field_type = qualified_field_type }
    )

let qualify_signature_exports = fun ~module_name ~type_decls exports ->
  let local_types = local_type_decl_index type_decls in
  exports |> List.map
    (
      fun (name, scheme) -> (name, qualify_scheme_with_local_types local_types ~module_name scheme)
    )

let qualify_type_decls_with_local_types = fun local_types ~module_name type_decls ->
  List.map
    (
      fun (type_decl: FileSummary.type_decl) ->
        let declaration = type_decl.declaration in
        let manifest =
          match declaration.manifest with
          | None -> None
          | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (qualify_type local_types module_name manifest_type))
          | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
              Some (
                TypeDecl.PolyVariant {
                  bound;
                  tags = tags |> List.map
                    (
                      fun (tag: TypeDecl.poly_variant_tag) ->
                        match tag.payload_type with
                        | Some payload_type -> { tag with payload_type = Some (qualify_type local_types module_name payload_type) }
                        | None -> tag
                    );
                  inherited = List.map (qualify_type local_types module_name) inherited
                }
              )
        in
        let constructors =
          declaration.constructors |> List.map
            (
              fun (constructor: TypeDecl.constructor) -> { constructor with scheme = qualify_scheme_with_local_types local_types ~module_name constructor.scheme; inline_record_labels = constructor.inline_record_labels |> Option.map (qualify_inline_record_labels local_types module_name) }
            )
        in
        let labels =
          declaration.labels |> List.map
            (
              fun (label: TypeDecl.label) ->
                let qualified_field_type = TypeScheme.map_type_preserving (qualify_type local_types module_name) label.field_type in
                if Std.Ptr.equal label.field_type qualified_field_type then
                  label
                else { label with field_type = qualified_field_type }
            )
        in
        { FileSummary.scope_path = SurfacePath.prepend_name module_name type_decl.scope_path; declaration = { declaration with manifest; constructors; labels } }
    )
    type_decls

let qualify_signature_type_decls = fun ~module_name type_decls ->
  let local_types = local_type_decl_index type_decls in
  List.map
    (
      fun (type_decl: FileSummary.type_decl) ->
        let qualified_decls = qualify_type_decls_with_local_types local_types ~module_name [ type_decl ] in
        match qualified_decls with
        | [ qualified_type_decl ] -> { qualified_type_decl with FileSummary.scope_path = type_decl.scope_path }
        | _ -> panic "expected one qualified type declaration"
    )
    type_decls

let rebind_export_target = fun ~from_module_name ~to_module_name target ->
  match target with
  | ModuleTypings.Site _ -> target
  | ModuleTypings.Export path ->
      let from_prefix = SurfacePath.of_name from_module_name in
      let to_prefix = SurfacePath.of_name to_module_name in
      let rebound_path =
        match SurfacePath.strip_prefix ~prefix:from_prefix path with
        | Some suffix -> SurfacePath.append_path to_prefix suffix
        | None -> path
      in
      ModuleTypings.Export rebound_path

let rebind_value_definition = fun ~from_module_name ~to_module_name (definition: ModuleTypings.value_definition) -> { definition with target = rebind_export_target ~from_module_name ~to_module_name definition.target }

let rebind_module_typings = fun ~module_name typings ->
  let type_decls = qualify_signature_type_decls ~module_name (ModuleTypings.type_decls typings) in
  let value_definitions = ModuleTypings.value_definitions typings |> List.map (rebind_value_definition ~from_module_name:(ModuleTypings.module_name typings) ~to_module_name:module_name) in
  match ModuleTypings.export_result typings with
  | FileSummary.TrustedExport { exports } ->
      let exports = qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result:(FileSummary.TrustedExport { exports }) ~type_decls ~value_definitions () in ModuleTypings.trusted ~module_name ~source_hash ~type_decls ~value_definitions exports
  | FileSummary.ErroredExport { exports } ->
      let exports = qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result:(FileSummary.ErroredExport { exports }) ~type_decls ~value_definitions () in ModuleTypings.errored ~module_name ~source_hash ~type_decls ~value_definitions exports
  | FileSummary.NoExport ->
      let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result:FileSummary.NoExport ~type_decls ~value_definitions () in ModuleTypings.missing ~module_name ~source_hash ~type_decls ~value_definitions ()

let qualify_surface = fun ~module_name ~type_decls exports ->
  let module_path = SurfacePath.of_name module_name in
  let local_types = local_type_decl_index type_decls in
  {
    exports = List.map
      (
        fun (name, scheme) -> (SurfacePath.append_path module_path name, qualify_scheme_with_local_types local_types ~module_name scheme)
      )
      exports;
    type_decls = qualify_type_decls_with_local_types local_types ~module_name type_decls
  }

let qualify_exports = fun ~module_name ~type_decls exports -> (qualify_surface ~module_name ~type_decls exports).exports

let qualify_type_decls = fun ~module_name type_decls -> (qualify_surface ~module_name ~type_decls []).type_decls
