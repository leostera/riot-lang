open Std
open Analysis
open Diagnostics
open Model

type t = {
  file: SemanticTree.file;
  config: TypConfig.t;
  regions: Region.t;
  mutable next_type_var_id: int;
  mutable next_binding_local_id: int;
  mutable next_hole_id: int;
  mutable diagnostics: Diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
  mutable visible_type_decls: FileSummary.type_decl list;
  visible_type_decl_by_path: (IdentPath.t, FileSummary.type_decl) Collections.HashMap.t;
  visible_type_decl_by_id: (TypeConstructorId.t, FileSummary.type_decl) Collections.HashMap.t;
  mutable forced_export_names: string list;
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

let same_named_type_constructor = fun left right ->
  match (left, right) with
  | (TypeRepr.Unresolved, TypeRepr.Unresolved) -> true
  | (TypeRepr.Resolved left, TypeRepr.Resolved right) -> TypeConstructorId.equal left right
  | _ -> false

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

let resolve_named_type_constructor_in_index = fun by_path type_constructor name ->
  match type_constructor with
  | TypeRepr.Resolved _ -> type_constructor
  | TypeRepr.Unresolved -> (
      match BuiltinTypeConstructors.of_path name with
      | TypeRepr.Resolved _ as resolved -> resolved
      | TypeRepr.Unresolved ->
          Collections.HashMap.get by_path name
          |> Option.map (fun (type_decl: FileSummary.type_decl) ->
            TypeRepr.Resolved type_decl.declaration.type_constructor_id)
          |> Option.unwrap_or ~default:TypeRepr.Unresolved
    )

let resolve_type_with = fun ~make ~resolve_named_type_constructor ->
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
    | TypeRepr.Named { type_constructor; name; arguments } ->
        let type_constructor' = resolve_named_type_constructor type_constructor name in
        let arguments' = map_preserving loop arguments in
        if same_named_type_constructor type_constructor type_constructor' && Std.Ptr.equal arguments arguments' then
          ty
        else
          make (TypeRepr.Named { type_constructor = type_constructor'; name; arguments = arguments' })
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

let annotate_type_decl_variances = fun type_decls ->
  let by_path = Collections.HashMap.with_capacity 32 in
  let by_id = Collections.HashMap.with_capacity 32 in
  let computed = Collections.HashMap.with_capacity 32 in
  let () =
    type_decls
    |> List.iter
      (fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in
        ())
  in
  let resolve_named_type_constructor = resolve_named_type_constructor_in_index by_path in
  let resolve_type = resolve_type_with
    ~make:TypeRepr.of_desc
    ~resolve_named_type_constructor in
  let rec parameter_variances_for_named_type visiting type_constructor_id name arguments =
    let default =
      List.map (fun _ -> TypeDecl.Invariant) arguments
    in
    let type_constructor_id =
      match resolve_named_type_constructor type_constructor_id name with
      | TypeRepr.Resolved type_constructor_id -> Some type_constructor_id
      | TypeRepr.Unresolved -> None
    in
    match type_constructor_id with
    | Some type_constructor_id when Collections.HashSet.contains visiting type_constructor_id ->
        default
    | Some type_constructor_id -> (
        match Collections.HashMap.get computed type_constructor_id with
        | Some variances -> variances
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
    | TypeRepr.Named { type_constructor; name; arguments } ->
        let parameter_variances = parameter_variances_for_named_type
          visiting
          type_constructor
          name
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
          collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type manifest_type)
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
              collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type inherited_type))
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
      (fun payload_type -> collect_type_variances_into visiting TypeDecl.Covariant variances (resolve_type payload_type)) in
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
          collect_type_variances_into visiting field_variance variances (resolve_type label.field_type))
    in
    declaration.param_ids |> List.map
      (fun param_id ->
        match Collections.HashMap.get variances param_id with
        | Some variance -> variance
        | None -> TypeDecl.Invariant)
  in
  type_decls |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      let param_variances = declaration_param_variances (Collections.HashSet.create ()) type_decl in
      { type_decl with declaration = { type_decl.declaration with param_variances } })

let type_decls_for_include = fun type_decls module_path -> aliases_for_type_decls type_decls module_path

let type_decls_for_module_alias = fun type_decls ~alias_name ~module_path ->
  aliases_for_type_decls type_decls module_path |> prefix_type_decls (IdentPath.of_name alias_name)

let rebuild_visible_type_decl_indexes = fun (state: t) ->
  let () = Collections.HashMap.clear state.visible_type_decl_by_path in
  let () = Collections.HashMap.clear state.visible_type_decl_by_id in
  state.visible_type_decls |> List.iter
    (fun (type_decl: FileSummary.type_decl) ->
      let _ = Collections.HashMap.insert state.visible_type_decl_by_path (type_decl_key type_decl) type_decl in
      let _ = Collections.HashMap.insert
        state.visible_type_decl_by_id
        type_decl.declaration.type_constructor_id
        type_decl in
      ())

let make = fun ~(config:TypConfig.t) file ->
  let visible_type_decls = annotate_type_decl_variances config.ambient_type_decls in
  let state = {
    file;
    config;
    regions = Region.create ();
    next_type_var_id = 0;
    next_binding_local_id = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
    visible_type_decls;
    visible_type_decl_by_path = Collections.HashMap.with_capacity 32;
    visible_type_decl_by_id = Collections.HashMap.with_capacity 32;
    forced_export_names = [];
  }
  in
  let () = rebuild_visible_type_decl_indexes state in
  state

let fresh_var = fun (state: t) ->
  let id = state.next_type_var_id in
  let () =
    state.next_type_var_id <- state.next_type_var_id + 1
  in
  Region.fresh_var state.regions id

let make_type = fun (state: t) desc -> TypeRepr.of_desc desc |> Region.track_node state.regions

let resolve_named_type_constructor = fun (state: t) type_constructor name ->
  resolve_named_type_constructor_in_index state.visible_type_decl_by_path type_constructor name

let resolve_type = fun (state: t) ->
  resolve_type_with
    ~make:(make_type state)
    ~resolve_named_type_constructor:(resolve_named_type_constructor state)

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
  let () =
    state.visible_type_decls <- bind_type_decls state.config.ambient_type_decls type_decls |> annotate_type_decl_variances
  in
  rebuild_visible_type_decl_indexes state
