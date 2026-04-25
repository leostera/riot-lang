open Std

type t = {
  type_decls: FileSummary.type_decl list;
  by_path: (SurfacePath.t, FileSummary.type_decl) Collections.HashMap.t;
  by_id: (TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t;
}

let qualify_name = fun scope_path name -> SurfacePath.append_name scope_path name

let type_decl_key = fun (type_decl: FileSummary.type_decl) -> qualify_name type_decl.scope_path type_decl.declaration.type_name

let bind_type_decls = fun type_decls introduced ->
  List.fold_left
    (
      fun acc (type_decl: FileSummary.type_decl) ->
        let key = type_decl_key type_decl in
        let acc =
          List.filter
            (
              fun candidate -> not (SurfacePath.equal (type_decl_key candidate) key)
            )
            acc
        in
        acc @ [ type_decl ]
    )
    type_decls
    introduced

let aliases_for_type_decls = fun type_decls module_path ->
  type_decls |> List.filter_map
    (
      fun (type_decl: FileSummary.type_decl) ->
        match SurfacePath.strip_prefix ~prefix:module_path type_decl.scope_path with
        | Some scope_path -> Some ({ type_decl with scope_path })
        | None -> None
    )

let prefix_type_decls = fun prefix type_decls ->
  List.map
    (
      fun (type_decl: FileSummary.type_decl) -> { type_decl with scope_path = SurfacePath.append_path prefix type_decl.scope_path }
    )
    type_decls

let map_preserving = fun loop xs ->
  let rec walk changed acc = function
    | [] ->
        if changed then
          List.rev acc
        else xs
    | x :: rest ->
        let mapped_x = loop x in walk (changed || not (Std.Ptr.equal x mapped_x)) (mapped_x :: acc) rest
  in
  walk false [] xs

let rebase_type_decl_refs = fun type_decls ->
  if List.is_empty type_decls then
    type_decls
  else
    let by_id = Collections.HashMap.with_capacity (List.length type_decls) in
    let () =
      type_decls |> List.iter
        (
          fun (type_decl: FileSummary.type_decl) ->
            let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in ()
        )
    in
    let rec rebase_type ty =
      let ty = TypeRepr.prune ty in
      match TypeRepr.view ty with
      | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ | TypeRepr.Var _ -> ty
      | TypeRepr.Option element ->
          let element2 = rebase_type element in
          if Std.Ptr.equal element element2 then
            ty
          else TypeRepr.option element2
      | TypeRepr.Result (ok_ty, error_ty) ->
          let ok_ty2 = rebase_type ok_ty in
          let error_ty2 = rebase_type error_ty in
          if Std.Ptr.equal ok_ty ok_ty2 && Std.Ptr.equal error_ty error_ty2 then
            ty
          else TypeRepr.result ok_ty2 error_ty2
      | TypeRepr.Array element ->
          let element2 = rebase_type element in
          if Std.Ptr.equal element element2 then
            ty
          else TypeRepr.array element2
      | TypeRepr.List element ->
          let element2 = rebase_type element in
          if Std.Ptr.equal element element2 then
            ty
          else TypeRepr.list element2
      | TypeRepr.Seq element ->
          let element2 = rebase_type element in
          if Std.Ptr.equal element element2 then
            ty
          else TypeRepr.seq element2
      | TypeRepr.Named { head; arguments } ->
          let arguments2 = map_preserving rebase_type arguments in
          let head2 =
            match Collections.HashMap.get by_id head.type_constructor_id with
            | Some (type_decl: FileSummary.type_decl) -> { head with name = type_decl_key type_decl }
            | None -> head
          in
          if Std.Ptr.equal arguments arguments2 && SurfacePath.equal head.name head2.name && TypeConstructorId.equal head.type_constructor_id head2.type_constructor_id then
            ty
          else TypeRepr.named ~head:head2 ~arguments:arguments2
      | TypeRepr.PolyVariant { bound; tags; inherited } ->
          let tags2 =
            map_preserving
              (
                fun (tag: TypeRepr.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type ->
                      let payload_type2 = rebase_type payload_type in
                      if Std.Ptr.equal payload_type payload_type2 then
                        tag
                      else { tag with payload_type = Some payload_type2 }
                  | None -> tag
              )
              tags
          in
          let inherited2 = map_preserving rebase_type inherited in
          if Std.Ptr.equal tags tags2 && Std.Ptr.equal inherited inherited2 then
            ty
          else TypeRepr.poly_variant ~bound ~tags:tags2 ~inherited:inherited2
      | TypeRepr.Tuple members ->
          let members2 = map_preserving rebase_type members in
          if Std.Ptr.equal members members2 then
            ty
          else TypeRepr.tuple members2
      | TypeRepr.Arrow { label; lhs; rhs } ->
          let lhs2 = rebase_type lhs in
          let rhs2 = rebase_type rhs in
          if Std.Ptr.equal lhs lhs2 && Std.Ptr.equal rhs rhs2 then
            ty
          else TypeRepr.arrow ~label ~lhs:lhs2 ~rhs:rhs2
      | TypeRepr.Package signature ->
          let values2 =
            map_preserving
              (
                fun (value: TypeRepr.package_value) ->
                  let scheme2 = TypeScheme.map_type_preserving rebase_type value.scheme in
                  if Std.Ptr.equal value.scheme scheme2 then
                    value
                  else { value with scheme = scheme2 }
              )
              signature.values
          in
          if Std.Ptr.equal signature.values values2 then
            ty
          else TypeRepr.package ~values:values2
    in
    let rebase_scheme scheme =
      let quantified, body = TypeScheme.to_explicit scheme in
      let body2 = rebase_type body in
      if Std.Ptr.equal body body2 then
        scheme
      else TypeScheme.of_explicit ~quantified body2
    in
    let rebase_labels labels =
      labels |> List.map
        (
          fun (label: TypeDecl.label) ->
            let field_type = TypeScheme.map_type_preserving rebase_type label.field_type in
            if Std.Ptr.equal label.field_type field_type then
              label
            else { label with field_type }
        )
    in
    type_decls |> List.map
      (
        fun (type_decl: FileSummary.type_decl) ->
          let declaration = type_decl.declaration in
          let manifest =
            match declaration.manifest with
            | None -> None
            | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (rebase_type manifest_type))
            | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
                Some (
                  TypeDecl.PolyVariant {
                    bound;
                    tags = tags |> List.map
                      (
                        fun (tag: TypeDecl.poly_variant_tag) ->
                          match tag.payload_type with
                          | Some payload_type -> { tag with payload_type = Some (rebase_type payload_type) }
                          | None -> tag
                      );
                    inherited = List.map rebase_type inherited
                  }
                )
          in
          let constructors =
            declaration.constructors |> List.map
              (
                fun (constructor: TypeDecl.constructor) -> { constructor with scheme = rebase_scheme constructor.scheme; inline_record_labels = constructor.inline_record_labels |> Option.map rebase_labels }
              )
          in
          let labels = rebase_labels declaration.labels in { type_decl with declaration = { declaration with manifest; constructors; labels } }
      )

let resolve_named_type_head_in_index = fun by_path name ->
  Collections.HashMap.get by_path name |> Option.map
    (
      fun (type_decl: FileSummary.type_decl) -> TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name
    ) |> fun resolved ->
    Option.or_else resolved
      (
        fun () -> BuiltinTypeConstructors.head_of_path name
      )

let resolve_named_type_decl_in_index = fun by_id by_path (head: TypeRepr.named_type_head) ->
  Option.or_else (Collections.HashMap.get by_id head.type_constructor_id)
    (
      fun () -> Collections.HashMap.get by_path head.name
    )

let nonrec_resolvers = fun by_id by_path (current: FileSummary.type_decl) ->
  let current_id = current.declaration.type_constructor_id in
  let not_current (type_decl: FileSummary.type_decl) = not (TypeConstructorId.equal type_decl.declaration.type_constructor_id current_id) in
  let resolve_named_type_head name =
    Collections.HashMap.get by_path name |> Option.filter not_current |> Option.map
      (
        fun (type_decl: FileSummary.type_decl) -> TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name
      ) |> fun resolved ->
      Option.or_else resolved
        (
          fun () -> BuiltinTypeConstructors.head_of_path name
        )
  in
  let resolve_named_type_decl (head: TypeRepr.named_type_head) =
    Option.or_else (Collections.HashMap.get by_id head.type_constructor_id |> Option.filter not_current)
      (
        fun () -> Collections.HashMap.get by_path head.name |> Option.filter not_current
      )
  in
  (resolve_named_type_head, resolve_named_type_decl)

let substitute_type_vars_with = fun ~make ty mapping ->
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ -> ty
    | TypeRepr.Option element ->
        let substituted_element = loop element in
        if Std.Ptr.equal element substituted_element then
          ty
        else make (TypeRepr.Option substituted_element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        let substituted_ok_ty = loop ok_ty in
        let substituted_error_ty = loop error_ty in
        if Std.Ptr.equal ok_ty substituted_ok_ty && Std.Ptr.equal error_ty substituted_error_ty then
          ty
        else make (TypeRepr.Result (substituted_ok_ty, substituted_error_ty))
    | TypeRepr.Array element ->
        let substituted_element = loop element in
        if Std.Ptr.equal element substituted_element then
          ty
        else make (TypeRepr.Array substituted_element)
    | TypeRepr.List element ->
        let substituted_element = loop element in
        if Std.Ptr.equal element substituted_element then
          ty
        else make (TypeRepr.List substituted_element)
    | TypeRepr.Seq element ->
        let substituted_element = loop element in
        if Std.Ptr.equal element substituted_element then
          ty
        else make (TypeRepr.Seq substituted_element)
    | TypeRepr.Named { head; arguments } ->
        let substituted_arguments = map_preserving loop arguments in
        if Std.Ptr.equal arguments substituted_arguments then
          ty
        else make (TypeRepr.Named { head; arguments = substituted_arguments })
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let substituted_tags =
          map_preserving
            (
              fun (tag: TypeRepr.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type ->
                    let substituted_payload_type = loop payload_type in
                    if Std.Ptr.equal payload_type substituted_payload_type then
                      tag
                    else { tag with payload_type = Some substituted_payload_type }
                | None -> tag
            )
            tags
        in
        let substituted_inherited = map_preserving loop inherited in
        if Std.Ptr.equal tags substituted_tags && Std.Ptr.equal inherited substituted_inherited then
          ty
        else make (TypeRepr.PolyVariant { bound; tags = substituted_tags; inherited = substituted_inherited })
    | TypeRepr.Tuple members ->
        let substituted_members = map_preserving loop members in
        if Std.Ptr.equal members substituted_members then
          ty
        else make (TypeRepr.Tuple substituted_members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let substituted_lhs = loop lhs in
        let substituted_rhs = loop rhs in
        if Std.Ptr.equal lhs substituted_lhs && Std.Ptr.equal rhs substituted_rhs then
          ty
        else make (TypeRepr.Arrow { label; lhs = substituted_lhs; rhs = substituted_rhs })
    | TypeRepr.Package signature ->
        let substituted_values =
          map_preserving
            (
              fun (value: TypeRepr.package_value) ->
                let substituted_scheme = TypeScheme.map_type_preserving loop value.scheme in
                if Std.Ptr.equal value.scheme substituted_scheme then
                  value
                else { value with scheme = substituted_scheme }
            )
            signature.values
        in
        if Std.Ptr.equal signature.values substituted_values then
          ty
        else TypeRepr.package ~values:substituted_values
    | TypeRepr.Var { id; link = None; _ } -> (
      match Collections.HashMap.get mapping id with
      | Some replacement -> replacement
      | None -> ty
    )
    | TypeRepr.Var { link = Some linked; _ } -> loop linked
  in
  loop ty

let instantiate_alias_manifest = fun ~make (type_decl: FileSummary.type_decl) arguments ->
  match type_decl.declaration.manifest with
  | Some (TypeDecl.Alias manifest_type) when List.length type_decl.declaration.param_ids = List.length arguments ->
      let mapping = Collections.HashMap.with_capacity 8 in
      let () =
        List.iter2
          (
            fun param_id argument ->
              let _ = Collections.HashMap.insert mapping param_id argument in ()
          )
          type_decl.declaration.param_ids
          arguments
      in
      Some (substitute_type_vars_with ~make manifest_type mapping)
  | _ -> None

let resolve_type_with = fun ~make ~resolve_named_type_decl ~resolve_named_type_head ->
  let same_head left right = TypeConstructorId.equal left.TypeRepr.type_constructor_id right.TypeRepr.type_constructor_id && SurfacePath.equal left.TypeRepr.name right.TypeRepr.name in
  let builtin_type_of_head (head: TypeRepr.named_type_head) arguments =
    Option.and_then (BuiltinTypeConstructors.head_of_path head.TypeRepr.name)
      (
        fun builtin_head ->
          if TypeConstructorId.equal builtin_head.TypeRepr.type_constructor_id head.TypeRepr.type_constructor_id then
            BuiltinTypeConstructors.type_of_path head.TypeRepr.name arguments
          else None
      )
  in
  let builtin_type_of_decl (type_decl: FileSummary.type_decl) (head: TypeRepr.named_type_head) arguments =
    if TypeConstructorId.equal head.TypeRepr.type_constructor_id type_decl.FileSummary.declaration.type_constructor_id then
      builtin_type_of_head head arguments
    else None
  in
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ | TypeRepr.Var _ -> ty
    | TypeRepr.Option element ->
        let resolved_element = loop element in
        if Std.Ptr.equal element resolved_element then
          ty
        else make (TypeRepr.Option resolved_element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        let resolved_ok_ty = loop ok_ty in
        let resolved_error_ty = loop error_ty in
        if Std.Ptr.equal ok_ty resolved_ok_ty && Std.Ptr.equal error_ty resolved_error_ty then
          ty
        else make (TypeRepr.Result (resolved_ok_ty, resolved_error_ty))
    | TypeRepr.Array element ->
        let resolved_element = loop element in
        if Std.Ptr.equal element resolved_element then
          ty
        else make (TypeRepr.Array resolved_element)
    | TypeRepr.List element ->
        let resolved_element = loop element in
        if Std.Ptr.equal element resolved_element then
          ty
        else make (TypeRepr.List resolved_element)
    | TypeRepr.Seq element ->
        let resolved_element = loop element in
        if Std.Ptr.equal element resolved_element then
          ty
        else make (TypeRepr.Seq resolved_element)
    | TypeRepr.Named { head; arguments } ->
        let resolved_arguments = map_preserving loop arguments in
        let resolved_head =
          match resolve_named_type_head head.name with
          | Some resolved_head -> resolved_head
          | None -> head
        in
        (
          match Option.or_else (resolve_named_type_decl resolved_head)
            (
              fun () -> resolve_named_type_decl head
            ) with
          | Some type_decl -> (
            match instantiate_alias_manifest ~make type_decl resolved_arguments with
            | Some manifest -> loop manifest
            | None -> (
              match builtin_type_of_decl type_decl resolved_head resolved_arguments with
              | Some builtin -> builtin
              | None ->
                  if Std.Ptr.equal arguments resolved_arguments && same_head head resolved_head then
                    ty
                  else make (TypeRepr.Named { head = resolved_head; arguments = resolved_arguments })
            )
          )
          | None -> (
            match builtin_type_of_head resolved_head resolved_arguments with
            | Some builtin -> builtin
            | None ->
                if Std.Ptr.equal arguments resolved_arguments && same_head head resolved_head then
                  ty
                else make (TypeRepr.Named { head = resolved_head; arguments = resolved_arguments })
          )
        )
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let resolved_tags =
          map_preserving
            (
              fun (tag: TypeRepr.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type ->
                    let resolved_payload_type = loop payload_type in
                    if Std.Ptr.equal payload_type resolved_payload_type then
                      tag
                    else { tag with payload_type = Some resolved_payload_type }
                | None -> tag
            )
            tags
        in
        let resolved_inherited = map_preserving loop inherited in
        if Std.Ptr.equal tags resolved_tags && Std.Ptr.equal inherited resolved_inherited then
          ty
        else make (TypeRepr.PolyVariant { bound; tags = resolved_tags; inherited = resolved_inherited })
    | TypeRepr.Tuple members ->
        let resolved_members = map_preserving loop members in
        if Std.Ptr.equal members resolved_members then
          ty
        else make (TypeRepr.Tuple resolved_members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let resolved_lhs = loop lhs in
        let resolved_rhs = loop rhs in
        if Std.Ptr.equal lhs resolved_lhs && Std.Ptr.equal rhs resolved_rhs then
          ty
        else make (TypeRepr.Arrow { label; lhs = resolved_lhs; rhs = resolved_rhs })
    | TypeRepr.Package signature ->
        let resolved_values =
          map_preserving
            (
              fun (value: TypeRepr.package_value) ->
                let resolved_scheme = TypeScheme.map_type_preserving loop value.scheme in
                if Std.Ptr.equal value.scheme resolved_scheme then
                  value
                else { value with scheme = resolved_scheme }
            )
            signature.values
        in
        if Std.Ptr.equal signature.values resolved_values then
          ty
        else make (TypeRepr.Package { values = resolved_values })
  in
  loop

let find_type_expansion = fun visible_types head -> resolve_named_type_decl_in_index visible_types.by_id visible_types.by_path head

let resolve_type = fun visible_types -> resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl:(find_type_expansion visible_types) ~resolve_named_type_head:(resolve_named_type_head_in_index visible_types.by_path)

let expand_head = fun visible_types ->
  let expand = resolve_type visible_types in
  fun ty ->
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Named _ | TypeRepr.Var { link = Some _; _ } -> expand ty
    | _ -> ty

let canonicalize_scheme_with = fun canonicalize_type scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  let canonical_body = canonicalize_type body in
  if Std.Ptr.equal body canonical_body then
    scheme
  else TypeScheme.of_explicit ~quantified canonical_body

let annotate_type_decl_variances = fun ?cached_by_id type_decls ->
  let by_path = Collections.HashMap.with_capacity 32 in
  let by_id = Collections.HashMap.with_capacity 32 in
  let computed = Collections.HashMap.with_capacity 32 in
  let cached_param_variances type_constructor_id =
    match cached_by_id with
    | Some cached_by_id ->
        Collections.HashMap.get cached_by_id type_constructor_id |> Option.map
          (
            fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.param_variances
          )
    | None -> None
  in
  let () =
    type_decls |> List.iter
      (
        fun (type_decl: FileSummary.type_decl) ->
          let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
          let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in ()
      )
  in
  let default_resolve_named_type_head = resolve_named_type_head_in_index by_path in
  let canonicalize_type_decl_heads (type_decl: FileSummary.type_decl) =
    let (resolve_named_type_head, resolve_named_type_decl) =
      if type_decl.declaration.nonrec_ then
        nonrec_resolvers by_id by_path type_decl
      else (default_resolve_named_type_head, fun _ -> None)
    in
    let resolve_type = resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl ~resolve_named_type_head in
    let declaration = type_decl.declaration in
    let manifest =
      match declaration.manifest with
      | None -> None
      | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (resolve_type manifest_type))
      | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
          Some (
            TypeDecl.PolyVariant {
              bound;
              tags = List.map
                (
                  fun (tag: TypeDecl.poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type -> { tag with payload_type = Some (resolve_type payload_type) }
                    | None -> tag
                )
                tags;
              inherited = List.map resolve_type inherited
            }
          )
    in
    let constructors =
      declaration.constructors |> List.map
        (
          fun (constructor: TypeDecl.constructor) ->
            let body = TypeScheme.body constructor.scheme in
            let resolved_body = resolve_type body in
            let inline_record_labels =
              constructor.inline_record_labels |> Option.map
                (
                  List.map
                    (
                      fun (label: TypeDecl.label) ->
                        let resolved_field_type = TypeScheme.map_type_preserving resolve_type label.field_type in
                        if Std.Ptr.equal label.field_type resolved_field_type then
                          label
                        else { label with field_type = resolved_field_type }
                    )
                )
            in
            if Std.Ptr.equal body resolved_body && Option.equal
              (
                fun left right -> List.for_all2 Std.Ptr.equal left right
              )
              constructor.inline_record_labels
              inline_record_labels then
              constructor
            else { constructor with scheme = TypeScheme.of_type resolved_body; inline_record_labels }
        )
    in
    let labels =
      declaration.labels |> List.map
        (
          fun (label: TypeDecl.label) ->
            let resolved_field_type = TypeScheme.map_type_preserving resolve_type label.field_type in
            if Std.Ptr.equal label.field_type resolved_field_type then
              label
            else { label with field_type = resolved_field_type }
        )
    in
    { type_decl with declaration = { declaration with manifest; constructors; labels } }
  in
  let rec parameter_variances_for_named_type visiting ~name ~type_constructor_id arguments =
    let default =
      List.map
        (
          fun _ -> TypeDecl.Invariant
        )
        arguments
    in
    match type_constructor_id with
    | Some type_constructor_id when Collections.HashSet.contains visiting type_constructor_id -> default
    | Some type_constructor_id -> (
      match Collections.HashMap.get computed type_constructor_id with
      | Some variances -> variances
      | None -> (
        match cached_param_variances type_constructor_id with
        | Some variances ->
            let _ = Collections.HashMap.insert computed type_constructor_id variances in variances
        | None -> (
          match Collections.HashMap.get by_id type_constructor_id with
          | Some type_decl ->
              let () = Collections.HashSet.insert visiting type_constructor_id |> ignore in
              let variances = declaration_param_variances visiting type_decl in
              let _ = Collections.HashSet.remove visiting type_constructor_id in
              let _ = Collections.HashMap.insert computed type_constructor_id variances in variances
          | None -> default
        )
      )
    )
    | None -> (
      match Collections.HashMap.get by_path name with
      | Some type_decl -> declaration_param_variances visiting type_decl
      | None -> default
    )
  and collect_type_variances_into visiting variance acc ty =
    match TypeRepr.view (TypeRepr.prune ty) with
    | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ -> ()
    | TypeRepr.Option element | TypeRepr.List element | TypeRepr.Seq element -> collect_type_variances_into visiting variance acc element
    | TypeRepr.Result (ok_ty, error_ty) ->
        let () = collect_type_variances_into visiting variance acc ok_ty in collect_type_variances_into visiting variance acc error_ty
    | TypeRepr.Array element -> collect_type_variances_into visiting TypeDecl.Invariant acc element
    | TypeRepr.Named { head = { type_constructor_id; name }; arguments } ->
        let parameter_variances = parameter_variances_for_named_type visiting ~name ~type_constructor_id:(Some type_constructor_id) arguments in
        let rec loop arguments parameter_variances =
          match arguments, parameter_variances with
          | (argument :: rest_arguments, parameter_variance :: rest_variances) ->
              let () = collect_type_variances_into visiting (TypeDecl.compose_variance variance parameter_variance) acc argument in loop rest_arguments rest_variances
          | _ -> ()
        in
        loop arguments parameter_variances
    | TypeRepr.PolyVariant { tags; inherited; _ } ->
        let () =
          tags |> List.iter
            (
              fun (tag: TypeRepr.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type -> collect_type_variances_into visiting variance acc payload_type
                | None -> ()
            )
        in
        List.iter
          (
            fun inherited_type -> collect_type_variances_into visiting variance acc inherited_type
          )
          inherited
    | TypeRepr.Tuple members ->
        List.iter
          (
            fun member -> collect_type_variances_into visiting variance acc member
          )
          members
    | TypeRepr.Arrow { lhs; rhs; _ } ->
        let () = collect_type_variances_into visiting (TypeDecl.flip_variance variance) acc lhs in collect_type_variances_into visiting variance acc rhs
    | TypeRepr.Package signature ->
        List.iter
          (
            fun (value: TypeRepr.package_value) -> collect_type_variances_into visiting variance acc (TypeScheme.body value.scheme)
          )
          signature.values
    | TypeRepr.Var var -> (
      match var.link with
      | Some linked -> collect_type_variances_into visiting variance acc linked
      | None ->
          match Collections.HashMap.get acc var.id with
          | Some existing ->
              let joined = TypeDecl.join_variance existing variance in
              if not (joined = existing) then
                let _ = Collections.HashMap.insert acc var.id joined in ()
          | None ->
              let _ = Collections.HashMap.insert acc var.id variance in ()
    )
  and declaration_param_variances visiting (type_decl: FileSummary.type_decl) =
    let declaration = type_decl.declaration in
    let (resolve_named_type_head, resolve_named_type_decl) =
      if declaration.nonrec_ then
        nonrec_resolvers by_id by_path type_decl
      else (default_resolve_named_type_head, resolve_named_type_decl_in_index by_id by_path)
    in
    let resolve_type = resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl ~resolve_named_type_head in
    let variances = Collections.HashMap.with_capacity 8 in
    let () =
      match declaration.manifest with
      | Some (TypeDecl.Alias manifest_type) -> collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type manifest_type)
      | Some (TypeDecl.PolyVariant { tags; inherited; _ }) ->
          let () =
            tags |> List.iter
              (
                fun (tag: TypeDecl.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type payload_type)
                  | None -> ()
              )
          in
          inherited |> List.iter
            (
              fun inherited_type -> collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type inherited_type)
            )
      | None -> ()
    in
    let constructor_payload_types =
      declaration.constructors |> List.concat_map
        (
          fun (constructor: TypeDecl.constructor) ->
            let rec loop acc ty =
              match TypeRepr.view (TypeRepr.prune ty) with
              | TypeRepr.Arrow { lhs; rhs; _ } -> loop (lhs :: acc) rhs
              | _ -> List.rev acc
            in
            loop [] (TypeScheme.body constructor.scheme)
        )
    in
    let () =
      constructor_payload_types |> List.iter
        (
          fun payload_type -> collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type payload_type)
        )
    in
    let () =
      declaration.labels |> List.iter
        (
          fun (label: TypeDecl.label) ->
            let field_variance =
              if label.mutable_ then
                TypeDecl.Invariant
              else TypeDecl.Covariant
            in
            collect_type_variances_into visiting field_variance variances (resolve_type (TypeScheme.body label.field_type))
        )
    in
    declaration.param_ids |> List.map
      (
        fun param_id ->
          match Collections.HashMap.get variances param_id with
          | Some variance -> variance
          | None -> TypeDecl.Invariant
      )
  in
  type_decls |> List.map
    (
      fun (type_decl: FileSummary.type_decl) ->
        let canonical_type_decl = canonicalize_type_decl_heads type_decl in
        let param_variances =
          match cached_param_variances canonical_type_decl.declaration.type_constructor_id with
          | Some param_variances -> param_variances
          | None -> declaration_param_variances (Collections.HashSet.create ()) canonical_type_decl
        in
        { canonical_type_decl with declaration = { canonical_type_decl.declaration with param_variances } }
    )

let empty = { type_decls = []; by_path = Collections.HashMap.with_capacity 32; by_id = Collections.HashMap.with_capacity 32 }

let index_type_decls = fun type_decls ->
  let by_path = Collections.HashMap.with_capacity (List.length type_decls + 16) in
  let by_id = Collections.HashMap.with_capacity (List.length type_decls + 16) in
  let () =
    type_decls |> List.iter
      (
        fun (type_decl: FileSummary.type_decl) ->
          let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
          let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in ()
      )
  in
  { type_decls; by_path; by_id }

let of_type_decls = fun ?cached_by_id type_decls -> type_decls |> annotate_type_decl_variances ?cached_by_id |> index_type_decls

let bind_annotated_type_decls = fun base introduced ->
  if List.is_empty introduced then
    base
  else bind_type_decls base.type_decls introduced |> index_type_decls

let merge = fun base introduced -> bind_annotated_type_decls base introduced.type_decls

let bind = fun base introduced -> introduced |> annotate_type_decl_variances ~cached_by_id:base.by_id |> bind_annotated_type_decls base

let type_decls = fun visible_types -> visible_types.type_decls

let by_id = fun visible_types -> visible_types.by_id

let lookup = fun visible_types name -> Collections.HashMap.get visible_types.by_path name

let lookup_by_id = fun visible_types type_constructor_id -> Collections.HashMap.get visible_types.by_id type_constructor_id

let resolve_named_type_head = fun visible_types name -> resolve_named_type_head_in_index visible_types.by_path name

let poly_variant_bound_matches_manifest = fun left right ->
  match left, right with
  | (TypeRepr.Exact, TypeDecl.Exact) | (TypeRepr.UpperBound, TypeDecl.UpperBound) | (TypeRepr.LowerBound, TypeDecl.LowerBound) -> true
  | _ -> false

let normalized_poly_variant_tags = fun tags ->
  tags |> List.map
    (
      fun (tag: TypeRepr.poly_variant_tag) -> (tag.name, tag.payload_type |> Option.map TypePrinter.type_to_string)
    ) |> List.sort compare

let normalized_manifest_tags = fun tags ->
  tags |> List.map
    (
      fun (tag: TypeDecl.poly_variant_tag) -> (tag.name, tag.payload_type |> Option.map TypePrinter.type_to_string)
    ) |> List.sort compare

let exact_poly_variant_alias = fun visible_types bound tags inherited ->
  if not (bound = TypeRepr.Exact) || not (List.is_empty inherited) then
    None
  else
    let normalized_tags = normalized_poly_variant_tags tags in
    visible_types.type_decls |> List.find_map
      (
        fun (type_decl: FileSummary.type_decl) ->
          if not (List.is_empty type_decl.declaration.param_ids) then
            None
          else
            match type_decl.declaration.manifest with
            | Some (TypeDecl.PolyVariant { bound; tags; inherited = [] }) when poly_variant_bound_matches_manifest TypeRepr.Exact bound && normalized_tags = normalized_manifest_tags tags -> Some type_decl
            | _ -> None
      )

let canonicalize_type = fun visible_types ->
  let expand_head = expand_head visible_types in
  let generation = TypeRepr.next_walk_generation () in
  let rec loop ty =
    let ty = expand_head ty |> TypeRepr.prune in
    if Int.equal ty.TypeRepr.walk_mark generation then
      ty
    else
      (
        ty.TypeRepr.walk_mark <- generation;
        match TypeRepr.view ty with
        | TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _ | TypeRepr.Var _ -> ty
        | TypeRepr.Option element ->
            let canonical_element = loop element in
            if Std.Ptr.equal element canonical_element then
              ty
            else TypeRepr.of_desc (TypeRepr.Option canonical_element)
        | TypeRepr.Result (ok_ty, error_ty) ->
            let canonical_ok_ty = loop ok_ty in
            let canonical_error_ty = loop error_ty in
            if Std.Ptr.equal ok_ty canonical_ok_ty && Std.Ptr.equal error_ty canonical_error_ty then
              ty
            else TypeRepr.of_desc (TypeRepr.Result (canonical_ok_ty, canonical_error_ty))
        | TypeRepr.Array element ->
            let canonical_element = loop element in
            if Std.Ptr.equal element canonical_element then
              ty
            else TypeRepr.of_desc (TypeRepr.Array canonical_element)
        | TypeRepr.List element ->
            let canonical_element = loop element in
            if Std.Ptr.equal element canonical_element then
              ty
            else TypeRepr.of_desc (TypeRepr.List canonical_element)
        | TypeRepr.Seq element ->
            let canonical_element = loop element in
            if Std.Ptr.equal element canonical_element then
              ty
            else TypeRepr.of_desc (TypeRepr.Seq canonical_element)
        | TypeRepr.Named { head; arguments } ->
            let canonical_arguments = map_preserving loop arguments in
            let canonical_head =
              match lookup_by_id visible_types head.type_constructor_id with
              | Some (type_decl: FileSummary.type_decl) -> TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name:(type_decl_key type_decl)
              | None -> resolve_named_type_head_in_index visible_types.by_path head.name |> Option.unwrap_or ~default:head
            in
            if Std.Ptr.equal arguments canonical_arguments && SurfacePath.equal head.name canonical_head.name && TypeConstructorId.equal head.type_constructor_id canonical_head.type_constructor_id then
              ty
            else TypeRepr.of_desc (TypeRepr.Named { head = canonical_head; arguments = canonical_arguments })
        | TypeRepr.PolyVariant { bound; tags; inherited } ->
            let canonical_tags =
              map_preserving
                (
                  fun (tag: TypeRepr.poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type ->
                        let canonical_payload_type = loop payload_type in
                        if Std.Ptr.equal payload_type canonical_payload_type then
                          tag
                        else { tag with payload_type = Some canonical_payload_type }
                    | None -> tag
                )
                tags
            in
            let canonical_inherited = map_preserving loop inherited in
            let ty =
              if Std.Ptr.equal tags canonical_tags && Std.Ptr.equal inherited canonical_inherited then
                ty
              else TypeRepr.of_desc (TypeRepr.PolyVariant { bound; tags = canonical_tags; inherited = canonical_inherited })
            in
            exact_poly_variant_alias visible_types bound canonical_tags canonical_inherited |> Option.map
              (
                fun (type_decl: FileSummary.type_decl) -> TypeRepr.named ~head:(TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name:(type_decl_key type_decl)) ~arguments:[]
              ) |> Option.unwrap_or ~default:ty
        | TypeRepr.Tuple members ->
            let canonical_members = map_preserving loop members in
            if Std.Ptr.equal members canonical_members then
              ty
            else TypeRepr.of_desc (TypeRepr.Tuple canonical_members)
        | TypeRepr.Arrow { label; lhs; rhs } ->
            let canonical_lhs = loop lhs in
            let canonical_rhs = loop rhs in
            if Std.Ptr.equal lhs canonical_lhs && Std.Ptr.equal rhs canonical_rhs then
              ty
            else TypeRepr.of_desc (TypeRepr.Arrow { label; lhs = canonical_lhs; rhs = canonical_rhs })
        | TypeRepr.Package signature ->
            let canonical_values =
              map_preserving
                (
                  fun (value: TypeRepr.package_value) ->
                    let canonical_scheme = TypeScheme.map_type_preserving loop value.scheme in
                    if Std.Ptr.equal value.scheme canonical_scheme then
                      value
                    else { value with scheme = canonical_scheme }
                )
                signature.values
            in
            if Std.Ptr.equal signature.values canonical_values then
              ty
            else TypeRepr.package ~values:canonical_values
      )
  in
  loop

let canonicalize_scheme = fun visible_types scheme -> canonicalize_scheme_with (canonicalize_type visible_types) scheme

let canonicalize_inline_record_labels = fun visible_types labels ->
  labels |> List.map
    (
      fun (label: TypeDecl.label) -> { label with field_type = canonicalize_scheme visible_types label.field_type }
    )

let canonicalize_type_decl = fun visible_types (type_decl: FileSummary.type_decl) ->
  let visible_types =
    if not type_decl.declaration.nonrec_ then
      visible_types
    else
      let type_decls =
        visible_types.type_decls |> List.filter
          (
            fun (candidate: FileSummary.type_decl) -> not (TypeConstructorId.equal candidate.declaration.type_constructor_id type_decl.declaration.type_constructor_id)
          )
      in
      let by_path = Collections.HashMap.with_capacity (List.length type_decls + 16) in
      let by_id = Collections.HashMap.with_capacity (List.length type_decls + 16) in
      let () =
        type_decls |> List.iter
          (
            fun (candidate: FileSummary.type_decl) ->
              let _ = Collections.HashMap.insert by_path (type_decl_key candidate) candidate in
              let _ = Collections.HashMap.insert by_id candidate.declaration.type_constructor_id candidate in ()
          )
      in
      { type_decls; by_path; by_id }
  in
  let declaration = type_decl.declaration in
  let manifest =
    match declaration.manifest with
    | None -> None
    | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (canonicalize_type visible_types manifest_type))
    | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
        Some (
          TypeDecl.PolyVariant {
            bound;
            tags = List.map
              (
                fun (tag: TypeDecl.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> { tag with payload_type = Some (canonicalize_type visible_types payload_type) }
                  | None -> tag
              )
              tags;
            inherited = List.map (canonicalize_type visible_types) inherited
          }
        )
  in
  let constructors =
    declaration.constructors |> List.map
      (
        fun (constructor: TypeDecl.constructor) -> { constructor with scheme = canonicalize_scheme visible_types constructor.scheme; inline_record_labels = constructor.inline_record_labels |> Option.map (canonicalize_inline_record_labels visible_types) }
      )
  in
  let labels =
    declaration.labels |> List.map
      (
        fun (label: TypeDecl.label) -> { label with field_type = canonicalize_scheme visible_types label.field_type }
      )
  in
  { type_decl with declaration = { declaration with manifest; constructors; labels } }

let type_decls_for_include = fun visible_types module_path -> aliases_for_type_decls visible_types.type_decls module_path |> rebase_type_decl_refs

let type_decls_for_module_alias = fun visible_types ~alias_name ~module_path -> aliases_for_type_decls visible_types.type_decls module_path |> prefix_type_decls (SurfacePath.of_name alias_name) |> rebase_type_decl_refs
