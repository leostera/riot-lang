open Std

type t = {
  type_decls: FileSummary.type_decl list;
  by_path: (IdentPath.t, FileSummary.type_decl) Collections.HashMap.t;
  by_id: (TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t;
}

let qualify_name = fun scope_path name ->
  IdentPath.append_name scope_path name

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  qualify_name type_decl.scope_path type_decl.declaration.type_name

let bind_type_decls = fun type_decls introduced ->
  List.fold_left
    (fun acc (type_decl: FileSummary.type_decl) ->
      let key = type_decl_key type_decl in
      let acc =
        List.filter (fun candidate -> not (IdentPath.equal (type_decl_key candidate) key)) acc
      in
      acc @ [ type_decl ])
    type_decls
    introduced

let aliases_for_type_decls = fun type_decls module_path ->
  type_decls |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      match IdentPath.strip_prefix ~prefix:module_path type_decl.scope_path with
      | Some scope_path -> Some { type_decl with scope_path }
      | None -> None)

let prefix_type_decls = fun prefix type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path })
    type_decls

let map_preserving = fun loop xs ->
  let rec walk changed acc = function
    | [] ->
        if changed then
          List.rev acc
        else
          xs
    | x :: rest ->
        let x' = loop x in
        walk (changed || not (Std.Ptr.equal x x')) (x' :: acc) rest
  in
  walk false [] xs

let resolve_named_type_head_in_index = fun by_path name ->
  let qualified_external_head =
    match IdentPath.to_segments name with
    | _ :: _ :: _ -> Some (TypeRepr.named_head
      ~type_constructor_id:(TypeConstructorId.of_path name)
      ~name)
    | _ -> None
  in
  Collections.HashMap.get by_path name
  |> Option.map
    (fun (type_decl: FileSummary.type_decl) ->
      TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name)
  |> fun resolved ->
    Option.or_else resolved
      (fun () ->
        Option.or_else
          (BuiltinTypeConstructors.head_of_path name)
          (fun () -> qualified_external_head))

let substitute_type_vars_with = fun ~make ty mapping ->
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Hole _ ->
        ty
    | TypeRepr.Option element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Option element')
    | TypeRepr.Result (ok_ty, error_ty) ->
        let ok_ty' = loop ok_ty in
        let error_ty' = loop error_ty in
        if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
          ty
        else
          make (TypeRepr.Result (ok_ty', error_ty'))
    | TypeRepr.Array element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Array element')
    | TypeRepr.List element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.List element')
    | TypeRepr.Seq element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Seq element')
    | TypeRepr.Named { head; arguments } ->
        let arguments' = map_preserving loop arguments in
        if Std.Ptr.equal arguments arguments' then
          ty
        else
          make (TypeRepr.Named { head; arguments = arguments' })
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let tags' = map_preserving
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type ->
                let payload_type' = loop payload_type in
                if Std.Ptr.equal payload_type payload_type' then
                  tag
                else
                  { tag with payload_type = Some payload_type' }
            | None -> tag)
          tags in
        let inherited' = map_preserving loop inherited in
        if Std.Ptr.equal tags tags' && Std.Ptr.equal inherited inherited' then
          ty
        else
          make (TypeRepr.PolyVariant { bound; tags = tags'; inherited = inherited' })
    | TypeRepr.Tuple members ->
        let members' = map_preserving loop members in
        if Std.Ptr.equal members members' then
          ty
        else
          make (TypeRepr.Tuple members')
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let lhs' = loop lhs in
        let rhs' = loop rhs in
        if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
          ty
        else
          make (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' })
    | TypeRepr.Var { id; link=None; _ } -> (
        match Collections.HashMap.get mapping id with
        | Some replacement -> replacement
        | None -> ty
      )
    | TypeRepr.Var { link=Some linked; _ } ->
        loop linked
  in
  loop ty

let instantiate_alias_manifest = fun ~make (type_decl: FileSummary.type_decl) arguments ->
  match type_decl.declaration.manifest with
  | Some (TypeDecl.Alias manifest_type) when List.length type_decl.declaration.param_ids
  = List.length arguments ->
      let mapping = Collections.HashMap.with_capacity 8 in
      let () =
        List.iter2
          (fun param_id argument ->
            let _ = Collections.HashMap.insert mapping param_id argument in
            ())
          type_decl.declaration.param_ids
          arguments
      in
      Some (substitute_type_vars_with ~make manifest_type mapping)
  | _ -> None

let resolve_type_with = fun ~make ~resolve_named_type_decl ~resolve_named_type_head ->
  let same_head = fun left right ->
    TypeConstructorId.equal left.TypeRepr.type_constructor_id right.TypeRepr.type_constructor_id
    && IdentPath.equal left.TypeRepr.name right.TypeRepr.name
  in
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Hole _
    | TypeRepr.Var _ ->
        ty
    | TypeRepr.Option element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Option element')
    | TypeRepr.Result (ok_ty, error_ty) ->
        let ok_ty' = loop ok_ty in
        let error_ty' = loop error_ty in
        if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
          ty
        else
          make (TypeRepr.Result (ok_ty', error_ty'))
    | TypeRepr.Array element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Array element')
    | TypeRepr.List element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.List element')
    | TypeRepr.Seq element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make (TypeRepr.Seq element')
    | TypeRepr.Named { head; arguments } ->
        let arguments' = map_preserving loop arguments in
        let resolved_head =
          match resolve_named_type_head head.name with
          | Some resolved_head -> resolved_head
          | None -> head
        in
        (
          match resolve_named_type_decl head.name with
          | Some type_decl -> (
              match instantiate_alias_manifest ~make type_decl arguments' with
              | Some manifest -> loop manifest
              | None -> (
                  match BuiltinTypeConstructors.type_of_path resolved_head.name arguments' with
                  | Some builtin -> builtin
                  | None ->
                  if Std.Ptr.equal arguments arguments' && same_head head resolved_head then
                    ty
                  else
                    make (TypeRepr.Named { head = resolved_head; arguments = arguments' })
                )
            )
          | None -> (
              match BuiltinTypeConstructors.type_of_path resolved_head.name arguments' with
              | Some builtin -> builtin
              | None ->
              if Std.Ptr.equal arguments arguments' && same_head head resolved_head then
                ty
              else
                make (TypeRepr.Named { head = resolved_head; arguments = arguments' })
            )
        )
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let tags' = map_preserving
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type ->
                let payload_type' = loop payload_type in
                if Std.Ptr.equal payload_type payload_type' then
                  tag
                else
                  { tag with payload_type = Some payload_type' }
            | None -> tag)
          tags in
        let inherited' = map_preserving loop inherited in
        if Std.Ptr.equal tags tags' && Std.Ptr.equal inherited inherited' then
          ty
        else
          make (TypeRepr.PolyVariant { bound; tags = tags'; inherited = inherited' })
    | TypeRepr.Tuple members ->
        let members' = map_preserving loop members in
        if Std.Ptr.equal members members' then
          ty
        else
          make (TypeRepr.Tuple members')
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let lhs' = loop lhs in
        let rhs' = loop rhs in
        if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
          ty
        else
          make (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' })
  in
  loop

let canonicalize_scheme_with = fun canonicalize_type scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  let body' = canonicalize_type body in
  if Std.Ptr.equal body body' then
    scheme
  else
    TypeScheme.of_explicit ~quantified body'

let annotate_type_decl_variances = fun ?cached_by_id type_decls ->
  let by_path = Collections.HashMap.with_capacity 32 in
  let by_id = Collections.HashMap.with_capacity 32 in
  let computed = Collections.HashMap.with_capacity 32 in
  let cached_param_variances type_constructor_id =
    match cached_by_id with
    | Some cached_by_id -> Collections.HashMap.get cached_by_id type_constructor_id
    |> Option.map (fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.param_variances)
    | None -> None
  in
  let () =
    type_decls
    |> List.iter
      (fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in
        ())
  in
  let resolve_named_type_head = resolve_named_type_head_in_index by_path in
  let resolve_type =
    resolve_type_with ~make:TypeRepr.of_desc
      ~resolve_named_type_decl:(fun name ->
        Collections.HashMap.get by_path name)
      ~resolve_named_type_head
  in
  let canonicalize_type_decl (type_decl: FileSummary.type_decl) =
    let declaration = type_decl.declaration in
    let manifest =
      match declaration.manifest with
      | None -> None
      | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (resolve_type manifest_type))
      | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
          Some (
            TypeDecl.PolyVariant {
              bound;
              tags =
                List.map
                  (fun (tag: TypeDecl.poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type -> {
                      tag
                      with payload_type = Some (resolve_type payload_type)
                    }
                    | None -> tag)
                  tags;
              inherited = List.map resolve_type inherited;
            }
          )
    in
    let constructors =
      declaration.constructors
      |> List.map
        (fun (constructor: TypeDecl.constructor) ->
          let body = TypeScheme.body constructor.scheme in
          let body' = resolve_type body in
          let inline_record_labels =
            constructor.inline_record_labels
            |> Option.map
              (List.map
                (fun (label: TypeDecl.label) ->
                  let field_type' = resolve_type label.field_type in
                  if Std.Ptr.equal label.field_type field_type' then
                    label
                  else
                    { label with field_type = field_type' }))
          in
          if Std.Ptr.equal body body'
            && Option.equal (fun left right -> List.for_all2 Std.Ptr.equal left right) constructor.inline_record_labels inline_record_labels
          then
            constructor
          else
            { constructor with scheme = TypeScheme.of_type body'; inline_record_labels })
    in
    let labels =
      declaration.labels
      |> List.map
        (fun (label: TypeDecl.label) ->
          let field_type' = resolve_type label.field_type in
          if Std.Ptr.equal label.field_type field_type' then
            label
          else
            { label with field_type = field_type' })
    in
    { type_decl with declaration = { declaration with manifest; constructors; labels } }
  in
  let rec parameter_variances_for_named_type visiting ~name ~type_constructor_id arguments =
    let default =
      List.map (fun _ -> TypeDecl.Invariant) arguments
    in
    match type_constructor_id with
    | Some type_constructor_id when Collections.HashSet.contains visiting type_constructor_id ->
        default
    | Some type_constructor_id -> (
        match Collections.HashMap.get computed type_constructor_id with
        | Some variances -> variances
        | None -> (
            match cached_param_variances type_constructor_id with
            | Some variances ->
                let _ = Collections.HashMap.insert computed type_constructor_id variances in
                variances
            | None -> (
                match Collections.HashMap.get by_id type_constructor_id with
                | Some type_decl ->
                    let () = Collections.HashSet.insert visiting type_constructor_id |> ignore in
                    let variances = declaration_param_variances visiting type_decl in
                    let _ = Collections.HashSet.remove visiting type_constructor_id in
                    let _ = Collections.HashMap.insert computed type_constructor_id variances in
                    variances
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
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Hole _ ->
        ()
    | TypeRepr.Option element
    | TypeRepr.List element
    | TypeRepr.Seq element ->
        collect_type_variances_into visiting variance acc element
    | TypeRepr.Result (ok_ty, error_ty) ->
        let () = collect_type_variances_into visiting variance acc ok_ty in
        collect_type_variances_into visiting variance acc error_ty
    | TypeRepr.Array element ->
        collect_type_variances_into visiting TypeDecl.Invariant acc element
    | TypeRepr.Named { head={ type_constructor_id; name }; arguments } ->
        let parameter_variances = parameter_variances_for_named_type
          visiting
          ~name
          ~type_constructor_id:(Some type_constructor_id)
          arguments in
        let rec loop arguments parameter_variances =
          match (arguments, parameter_variances) with
          | (argument :: rest_arguments, parameter_variance :: rest_variances) ->
              let () = collect_type_variances_into
                visiting
                (TypeDecl.compose_variance variance parameter_variance)
                acc
                argument in
              loop rest_arguments rest_variances
          | _ -> ()
        in
        loop arguments parameter_variances
    | TypeRepr.PolyVariant { tags; inherited; _ } ->
        let () =
          tags |> List.iter
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  collect_type_variances_into visiting variance acc payload_type
              | None -> ())
        in
        List.iter (fun inherited_type -> collect_type_variances_into visiting variance acc inherited_type) inherited
    | TypeRepr.Tuple members ->
        List.iter (fun member -> collect_type_variances_into visiting variance acc member) members
    | TypeRepr.Arrow { lhs; rhs; _ } ->
        let () = collect_type_variances_into visiting (TypeDecl.flip_variance variance) acc lhs in
        collect_type_variances_into visiting variance acc rhs
    | TypeRepr.Var var -> (
        match var.link with
        | Some linked -> collect_type_variances_into visiting variance acc linked
        | None ->
            match Collections.HashMap.get acc var.id with
            | Some existing ->
                let joined = TypeDecl.join_variance existing variance in
                if not (joined = existing) then
                  let _ = Collections.HashMap.insert acc var.id joined in
                  ()
            | None ->
                let _ = Collections.HashMap.insert acc var.id variance in
                ()
      )
  and declaration_param_variances visiting (type_decl: FileSummary.type_decl) =
    let declaration = type_decl.declaration in
    let variances = Collections.HashMap.with_capacity 8 in
    let () =
      match declaration.manifest with
      | Some (TypeDecl.Alias manifest_type) ->
          collect_type_variances_into
            visiting
            TypeDecl.Covariant
            variances
            (resolve_type manifest_type)
      | Some (TypeDecl.PolyVariant { tags; inherited; _ }) ->
          let () =
            tags
            |> List.iter
              (fun (tag: TypeDecl.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type -> collect_type_variances_into
                  visiting
                  TypeDecl.Covariant
                  variances
                  (resolve_type payload_type)
                | None -> ())
          in
          inherited
          |> List.iter
            (fun inherited_type ->
              collect_type_variances_into
                visiting
                TypeDecl.Covariant
                variances
                (resolve_type inherited_type))
      | None ->
          ()
    in
    let constructor_payload_types =
      declaration.constructors
      |> List.concat_map
        (fun (constructor: TypeDecl.constructor) ->
          let rec loop acc ty =
            match TypeRepr.view (TypeRepr.prune ty) with
            | TypeRepr.Arrow { lhs; rhs; _ } -> loop (lhs :: acc) rhs
            | _ -> List.rev acc
          in
          loop [] (TypeScheme.body constructor.scheme))
    in
    let () = constructor_payload_types
    |> List.iter
      (fun payload_type ->
        collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type payload_type)) in
    let () =
      declaration.labels
      |> List.iter
        (fun (label: TypeDecl.label) ->
          let field_variance =
            if label.mutable_ then
              TypeDecl.Invariant
            else
              TypeDecl.Covariant
          in
          collect_type_variances_into
            visiting
            field_variance
            variances
            (resolve_type label.field_type))
    in
    declaration.param_ids |> List.map
      (fun param_id ->
        match Collections.HashMap.get variances param_id with
        | Some variance -> variance
        | None -> TypeDecl.Invariant)
  in
  type_decls |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      let canonical_type_decl = canonicalize_type_decl type_decl in
      let param_variances =
        match cached_param_variances canonical_type_decl.declaration.type_constructor_id with
        | Some param_variances -> param_variances
        | None -> declaration_param_variances (Collections.HashSet.create ()) canonical_type_decl
      in
      {
        canonical_type_decl
        with declaration = { canonical_type_decl.declaration with param_variances }
      })

let empty = {
  type_decls = [];
  by_path = Collections.HashMap.with_capacity 32;
  by_id = Collections.HashMap.with_capacity 32
}

let of_type_decls = fun ?cached_by_id type_decls ->
  let type_decls = annotate_type_decl_variances ?cached_by_id type_decls in
  let by_path = Collections.HashMap.with_capacity (List.length type_decls + 16) in
  let by_id = Collections.HashMap.with_capacity (List.length type_decls + 16) in
  let () =
    type_decls
    |> List.iter
      (fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in
        ())
  in
  { type_decls; by_path; by_id }

let merge = fun base introduced ->
  let combined = bind_type_decls base.type_decls introduced.type_decls in
  of_type_decls ~cached_by_id:base.by_id combined

let bind = fun base introduced -> merge base (of_type_decls ~cached_by_id:base.by_id introduced)

let type_decls = fun visible_types -> visible_types.type_decls

let by_id = fun visible_types -> visible_types.by_id

let lookup = fun visible_types name ->
  Collections.HashMap.get visible_types.by_path name

let lookup_by_id = fun visible_types type_constructor_id ->
  Collections.HashMap.get visible_types.by_id type_constructor_id

let resolve_named_type_head = fun visible_types name ->
  resolve_named_type_head_in_index visible_types.by_path name

let poly_variant_bound_matches_manifest = fun left right ->
  match (left, right) with
  | (TypeRepr.Exact, TypeDecl.Exact)
  | (TypeRepr.UpperBound, TypeDecl.UpperBound)
  | (TypeRepr.LowerBound, TypeDecl.LowerBound) -> true
  | _ -> false

let normalized_poly_variant_tags = fun tags ->
  tags
  |> List.map
    (fun (tag: TypeRepr.poly_variant_tag) ->
      (
        tag.name,
        tag.payload_type |> Option.map TypePrinter.type_to_string
      ))
  |> List.sort compare

let normalized_manifest_tags = fun tags ->
  tags
  |> List.map
    (fun (tag: TypeDecl.poly_variant_tag) ->
      (
        tag.name,
        tag.payload_type |> Option.map TypePrinter.type_to_string
      ))
  |> List.sort compare

let exact_poly_variant_alias = fun visible_types bound tags inherited ->
  if not (bound = TypeRepr.Exact) || not (List.is_empty inherited) then
    None
  else
    let normalized_tags = normalized_poly_variant_tags tags in
    visible_types.type_decls |> List.find_map
      (fun (type_decl: FileSummary.type_decl) ->
        if not (List.is_empty type_decl.declaration.param_ids) then
          None
        else
          match type_decl.declaration.manifest with
          | Some (TypeDecl.PolyVariant { bound; tags; inherited = [] })
            when poly_variant_bound_matches_manifest TypeRepr.Exact bound
              && normalized_tags = normalized_manifest_tags tags ->
              Some type_decl
          | _ -> None)

let canonicalize_type = fun visible_types ->
  let resolve_type = resolve_type_with
    ~make:TypeRepr.of_desc
    ~resolve_named_type_decl:(lookup visible_types)
    ~resolve_named_type_head:(resolve_named_type_head visible_types) in
  let rec loop ty =
    let ty = resolve_type ty |> TypeRepr.prune in
    match TypeRepr.view ty with
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Hole _
    | TypeRepr.Var _ ->
        ty
    | TypeRepr.Option element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Option element')
    | TypeRepr.Result (ok_ty, error_ty) ->
        let ok_ty' = loop ok_ty in
        let error_ty' = loop error_ty in
        if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Result (ok_ty', error_ty'))
    | TypeRepr.Array element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Array element')
    | TypeRepr.List element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.List element')
    | TypeRepr.Seq element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Seq element')
    | TypeRepr.Named { head; arguments } ->
        let arguments' = map_preserving loop arguments in
        let head' =
          lookup_by_id visible_types head.type_constructor_id
          |> Option.map
            (fun (type_decl: FileSummary.type_decl) ->
              { head with name = type_decl_key type_decl })
          |> Option.unwrap_or ~default:head
        in
        if Std.Ptr.equal arguments arguments' && IdentPath.equal head.name head'.name then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Named { head = head'; arguments = arguments' })
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let tags' = map_preserving
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type ->
                let payload_type' = loop payload_type in
                if Std.Ptr.equal payload_type payload_type' then
                  tag
                else
                  { tag with payload_type = Some payload_type' }
            | None -> tag)
          tags in
        let inherited' = map_preserving loop inherited in
        let ty =
          if Std.Ptr.equal tags tags' && Std.Ptr.equal inherited inherited' then
            ty
          else
            TypeRepr.of_desc (TypeRepr.PolyVariant { bound; tags = tags'; inherited = inherited' })
        in
        exact_poly_variant_alias visible_types bound tags' inherited'
        |> Option.map
          (fun (type_decl: FileSummary.type_decl) ->
            TypeRepr.named
              ~head:(TypeRepr.named_head
                ~type_constructor_id:type_decl.declaration.type_constructor_id
                ~name:(type_decl_key type_decl))
              ~arguments:[])
        |> Option.unwrap_or ~default:ty
    | TypeRepr.Tuple members ->
        let members' = map_preserving loop members in
        if Std.Ptr.equal members members' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Tuple members')
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let lhs' = loop lhs in
        let rhs' = loop rhs in
        if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
          ty
        else
          TypeRepr.of_desc (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' })
  in
  loop

let canonicalize_scheme = fun visible_types scheme ->
  canonicalize_scheme_with (canonicalize_type visible_types) scheme

let canonicalize_inline_record_labels = fun visible_types labels ->
  labels
  |> List.map
    (fun (label: TypeDecl.label) ->
      {
        label with
        field_type = canonicalize_type visible_types label.field_type
      })

let canonicalize_type_decl = fun visible_types (type_decl: FileSummary.type_decl) ->
  let declaration = type_decl.declaration in
  let manifest =
    match declaration.manifest with
    | None -> None
    | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (canonicalize_type visible_types manifest_type))
    | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
        Some (
          TypeDecl.PolyVariant {
            bound;
            tags =
              List.map
                (fun (tag: TypeDecl.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> {
                    tag
                    with payload_type = Some (canonicalize_type visible_types payload_type)
                  }
                  | None -> tag)
                tags;
            inherited = List.map (canonicalize_type visible_types) inherited;
          }
        )
  in
  let constructors = declaration.constructors
  |> List.map
    (fun (constructor: TypeDecl.constructor) ->
      {
        constructor with
        scheme = canonicalize_scheme visible_types constructor.scheme;
        inline_record_labels =
          constructor.inline_record_labels
          |> Option.map (canonicalize_inline_record_labels visible_types)
      }) in
  let labels = declaration.labels
  |> List.map
    (fun (label: TypeDecl.label) ->
      { label with field_type = canonicalize_type visible_types label.field_type }) in
  { type_decl with declaration = { declaration with manifest; constructors; labels } }

let type_decls_for_include = fun visible_types module_path ->
  aliases_for_type_decls visible_types.type_decls module_path

let type_decls_for_module_alias = fun visible_types ~alias_name ~module_path ->
  aliases_for_type_decls visible_types.type_decls module_path
  |> prefix_type_decls (IdentPath.of_name alias_name)
