open Std
open Analysis
open Diagnostics
open Model

type t = {
  file: SemanticTree.file;
  config: TypConfig.t;
  solver: Solver.t;
  mutable next_binding_local_id: int;
  mutable next_hole_id: int;
  mutable diagnostics: Diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
  mutable visible_types: VisibleTypes.t;
  mutable forced_export_names: string list;
}

let qualify_name = fun scope_path name ->
  IdentPath.append_name scope_path name

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  qualify_name type_decl.scope_path type_decl.declaration.type_name

let bind_type_decls = fun type_decls introduced ->
  List.fold_left
    (fun acc (type_decl: FileSummary.type_decl) ->
      let key = qualify_name type_decl.scope_path type_decl.declaration.type_name in
      let acc =
        List.filter
          (fun (candidate: FileSummary.type_decl) ->
            not
              (IdentPath.equal (qualify_name candidate.scope_path candidate.declaration.type_name) key))
          acc
      in
      acc @ [ type_decl ])
    type_decls
    introduced

let prefix_type_decls = fun prefix type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path })
    type_decls

let same_named_head = fun left right ->
  TypeConstructorId.equal left.TypeRepr.type_constructor_id right.TypeRepr.type_constructor_id

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
  Collections.HashMap.get by_path name
  |> Option.map
    (fun (type_decl: FileSummary.type_decl) ->
      TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name)
  |> fun resolved ->
    Option.or_else resolved (fun () -> BuiltinTypeConstructors.head_of_path name)

let resolve_named_type_decl_in_index = fun by_path name ->
  Collections.HashMap.get by_path name

let nonrec_resolvers = fun by_path (type_decl: FileSummary.type_decl) ->
  let current_id = type_decl.declaration.type_constructor_id in
  let not_current (candidate: FileSummary.type_decl) = not
    (TypeConstructorId.equal candidate.declaration.type_constructor_id current_id) in
  let resolve_named_type_head name =
    Collections.HashMap.get by_path name
    |> Option.filter not_current
    |> Option.map
      (fun (candidate: FileSummary.type_decl) ->
        TypeRepr.named_head ~type_constructor_id:candidate.declaration.type_constructor_id ~name)
    |> fun resolved ->
      Option.or_else resolved (fun () -> BuiltinTypeConstructors.head_of_path name)
  in
  let resolve_named_type_decl name = Collections.HashMap.get by_path name |> Option.filter not_current in
  (resolve_named_type_head, resolve_named_type_decl)

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
        let tags' =
          map_preserving
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let payload_type' = loop payload_type in
                  if Std.Ptr.equal payload_type payload_type' then
                    tag
                  else
                    { tag with payload_type = Some payload_type' }
              | None -> tag)
            tags
        in
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
    | TypeRepr.Package signature ->
        let values' =
          map_preserving
            (fun (value: TypeRepr.package_value) ->
              let scheme' = loop value.scheme in
              if Std.Ptr.equal value.scheme scheme' then
                value
              else
                { value with scheme = scheme' })
            signature.values
        in
        if Std.Ptr.equal signature.values values' then
          ty
        else
          TypeRepr.package ~values:values'
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
  let same_head left right =
    TypeConstructorId.equal left.TypeRepr.type_constructor_id right.TypeRepr.type_constructor_id
    && IdentPath.equal left.TypeRepr.name right.TypeRepr.name in
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
        let tags' =
          map_preserving
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let payload_type' = loop payload_type in
                  if Std.Ptr.equal payload_type payload_type' then
                    tag
                  else
                    { tag with payload_type = Some payload_type' }
              | None -> tag)
            tags
        in
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
    | TypeRepr.Package signature ->
        let values' =
          map_preserving
            (fun (value: TypeRepr.package_value) ->
              let scheme' = loop value.scheme in
              if Std.Ptr.equal value.scheme scheme' then
                value
              else
                { value with scheme = scheme' })
            signature.values
        in
        if Std.Ptr.equal signature.values values' then
          ty
        else
          make (TypeRepr.Package { values = values' })
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
  let default_resolve_named_type_head = resolve_named_type_head_in_index by_path in
  let canonicalize_type_decl (type_decl: FileSummary.type_decl) =
    let (resolve_named_type_head, resolve_named_type_decl) =
      if type_decl.declaration.nonrec_ then
        nonrec_resolvers by_path type_decl
      else
        (default_resolve_named_type_head, resolve_named_type_decl_in_index by_path)
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
              (
                List.map
                  (fun (label: TypeDecl.label) ->
                    let field_type' = resolve_type label.field_type in
                    if Std.Ptr.equal label.field_type field_type' then
                      label
                    else
                      { label with field_type = field_type' })
              )
          in
          if Std.Ptr.equal body body' && Option.equal
              (fun left right ->
                List.for_all2 Std.Ptr.equal left right)
              constructor.inline_record_labels
              inline_record_labels then
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
          tags
          |> List.iter
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type -> collect_type_variances_into visiting variance acc payload_type
              | None -> ())
        in
        List.iter
          (fun inherited_type -> collect_type_variances_into visiting variance acc inherited_type)
          inherited
    | TypeRepr.Tuple members ->
        List.iter (fun member -> collect_type_variances_into visiting variance acc member) members
    | TypeRepr.Arrow { lhs; rhs; _ } ->
        let () = collect_type_variances_into visiting (TypeDecl.flip_variance variance) acc lhs in
        collect_type_variances_into visiting variance acc rhs
    | TypeRepr.Package signature ->
        List.iter
          (fun (value: TypeRepr.package_value) ->
            collect_type_variances_into visiting variance acc value.scheme)
          signature.values
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
    let (resolve_named_type_head, resolve_named_type_decl) =
      if declaration.nonrec_ then
        nonrec_resolvers by_path type_decl
      else
        (default_resolve_named_type_head, resolve_named_type_decl_in_index by_path)
    in
    let resolve_type = resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl ~resolve_named_type_head in
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

let type_decls_for_include = fun visible_types module_path ->
  VisibleTypes.type_decls_for_include visible_types module_path

let type_decls_for_module_alias = fun visible_types ~alias_name ~module_path ->
  VisibleTypes.type_decls_for_module_alias visible_types ~alias_name ~module_path

let make = fun ~(config:TypConfig.t) file ->
  let state = {
    file;
    config;
    solver = Solver.create ();
    next_binding_local_id = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
    visible_types = config.ambient_visible_types;
    forced_export_names = [];
  }
  in
  state

let fresh_var = fun (state: t) -> Solver.fresh_var state.solver

let make_type = fun (state: t) desc ->
  Solver.make_type state.solver desc

let resolve_named_type_head = fun (state: t) name ->
  VisibleTypes.resolve_named_type_head state.visible_types name

let canonicalize_type = fun (state: t) -> VisibleTypes.canonicalize_type state.visible_types

let canonicalize_scheme = fun (state: t) scheme ->
  canonicalize_scheme_with (canonicalize_type state) scheme

let canonicalize_scheme_with_name_resolution = fun ~resolve_named_type_decl ~resolve_named_type_head scheme ->
  canonicalize_scheme_with
    (resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl ~resolve_named_type_head)
    scheme

let canonicalize_type_decl_with_name_resolution = fun ~resolve_named_type_decl ~resolve_named_type_head (
  type_decl: FileSummary.type_decl
) ->
  let canonicalize_type = resolve_type_with ~make:TypeRepr.of_desc ~resolve_named_type_decl ~resolve_named_type_head in
  let canonicalize_inline_record_labels labels = labels
  |> List.map
    (fun (label: TypeDecl.label) -> { label with field_type = canonicalize_type label.field_type }) in
  let declaration = type_decl.declaration in
  let manifest =
    match declaration.manifest with
    | None -> None
    | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (canonicalize_type manifest_type))
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
                    with payload_type = Some (canonicalize_type payload_type)
                  }
                  | None -> tag)
                tags;
            inherited = List.map canonicalize_type inherited;
          }
        )
  in
  let constructors = declaration.constructors
  |> List.map
    (fun (constructor: TypeDecl.constructor) ->
      {
        constructor
        with scheme = canonicalize_scheme_with canonicalize_type constructor.scheme;
        inline_record_labels = constructor.inline_record_labels |> Option.map canonicalize_inline_record_labels
      }) in
  let labels = declaration.labels
  |> List.map
    (fun (label: TypeDecl.label) -> { label with field_type = canonicalize_type label.field_type }) in
  { type_decl with declaration = { declaration with manifest; constructors; labels } }

let canonicalize_scheme_with_named_type_head = fun resolve_named_type_head scheme ->
  canonicalize_scheme_with_name_resolution
    ~resolve_named_type_decl:(fun _ -> None)
    ~resolve_named_type_head
    scheme

let visible_type_decls = fun (state: t) -> VisibleTypes.type_decls state.visible_types

let visible_type_decl = fun (state: t) name ->
  VisibleTypes.lookup state.visible_types name

let visible_type_decl_by_id = fun (state: t) type_constructor_id ->
  VisibleTypes.lookup_by_id state.visible_types type_constructor_id

let fresh_binding_local_id = fun (state: t) ->
  let local_id = state.next_binding_local_id in
  let () =
    state.next_binding_local_id <- state.next_binding_local_id + 1
  in
  local_id

let fresh_hole = fun (state: t) ->
  let hole_id = state.next_hole_id in
  let () =
    state.next_hole_id <- state.next_hole_id + 1
  in
  make_type state (TypeRepr.Hole hole_id)

let set_visible_type_decls = fun (state: t) type_decls ->
  let local_visible_types = VisibleTypes.of_type_decls
    ~cached_by_id:(VisibleTypes.by_id state.visible_types)
    type_decls in
  let () =
    state.visible_types <- VisibleTypes.merge state.config.ambient_visible_types local_visible_types
  in
  VisibleTypes.type_decls local_visible_types
