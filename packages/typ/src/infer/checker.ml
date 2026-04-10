open Std
open Analysis
open Diagnostics
open Model
module Typ_diagnostic = Diagnostic
module Solver = Solver
open State

(* Use Super since open Std brings an Env module of its own that shadows ours *)

module Env = Super.Env
open Env

type t = {
  exports: Check_result.env;
  export_bindings: Check_result.binding_ref list;
  type_decls: FileSummary.type_decl list;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Typ_diagnostic.t list;
}

type record_type_decl = Env.Label_env.record_decl

type named_owner = {
  type_constructor_id: TypeConstructorId.t;
}

type variance = TypeDecl.variance

type state = State.t

type pattern_bindings = {
  entries: Binding.t list;
  module_entries: (string * Env.t) list;
}

module Label_name_map = Collections.Map.Make (String)

let empty_span = Syn.Ceibo.Span.make ~start:0 ~end_:0

let missing_record_decl_param_hole_id = (-200)

let qualify_name = State.qualify_name

let has_surface_entries = fun env visible_types module_path ->
  not (List.is_empty (Env.names (Env.entries_for_include env module_path)))
  || not (List.is_empty (type_decls_for_include visible_types module_path))

let resolve_module_path_in_scope = fun env visible_types scope_path module_path ->
  if IdentPath.is_empty scope_path || not (IdentPath.is_bare module_path) then
    module_path
  else
    let scoped_module_path = IdentPath.append_path scope_path module_path in
    if has_surface_entries env visible_types module_path then
      module_path
    else if has_surface_entries env visible_types scoped_module_path then
      scoped_module_path
    else
      module_path

let bind_type_decls = State.bind_type_decls

let type_decls_for_include = State.type_decls_for_include

let type_decls_for_module_alias = State.type_decls_for_module_alias

let make_state = State.make

let canonicalize_type = State.canonicalize_type

let canonicalize_scheme = State.canonicalize_scheme

let visible_type_decls = State.visible_type_decls

let fresh_var = fun (state: state) -> Solver.fresh_var state.solver

let fresh_rigid_var = State.fresh_rigid_var

let fresh_binding_ident = fun state name ->
  Binding.make_ident ~local_id:(State.fresh_binding_local_id state) ~name

let fresh_hole = State.fresh_hole

let make_type = fun (state: state) desc ->
  Solver.make_type state.solver desc

let set_visible_type_decls = State.set_visible_type_decls

let empty_pattern_bindings = { entries = []; module_entries = [] }

let pattern_bindings_of_entries entries = { empty_pattern_bindings with entries }

let merge_pattern_bindings = fun left right ->
  {
    entries = left.entries @ right.entries;
    module_entries = left.module_entries @ right.module_entries
  }

let env_with_pattern_bindings = fun env bindings ->
  let env = Env.extend env bindings.entries in
  bindings.module_entries |> List.fold_left
    (fun env (name, module_env) ->
      Env.bind env (Env.singleton_module ~name module_env))
    env

let scheme_body = TypeScheme.body

let prelude_env = fun (state: state) (config: TypConfig.t) ->
  Env.bind
    (Env.of_entries ~make_ident:(fresh_binding_ident state) ~provenance:Binding.Prelude config.prelude)
    (Env.of_type_decls LanguagePrelude.type_decls)

let ambient_env = fun (state: state) (config: TypConfig.t) ->
  let ambient_entries = config.ambient
  |> List.map (fun (name, scheme) -> (name, canonicalize_scheme state scheme)) in
  Env.of_entries ~make_ident:(fresh_binding_ident state) ~provenance:Binding.Ambient ambient_entries

let ambient_type_env = fun (state: state) (_config: TypConfig.t) ->
  Env.of_type_decls (visible_type_decls state)

let static_ident_generator = fun () ->
  let next_local_id = ref (-1) in
  fun name ->
    let local_id = !next_local_id in
    next_local_id := local_id - 1;
    Binding.make_ident ~local_id ~name

let initial_env_of_config = fun ~(config:TypConfig.t) ->
  let make_ident = static_ident_generator () in
  let visible_types = config.ambient_visible_types in
  let prelude = config.prelude
  |> List.map (fun (path, scheme) -> (path, VisibleTypes.canonicalize_scheme visible_types scheme)) in
  let ambient = config.ambient
  |> List.map (fun (path, scheme) -> (path, VisibleTypes.canonicalize_scheme visible_types scheme)) in
  Env.bind
    (Env.bind
      (Env.of_entries ~make_ident ~provenance:Binding.Prelude prelude)
      (Env.of_entries ~make_ident ~provenance:Binding.Ambient ambient))
    (Env.of_type_decls config.ambient_type_decls)

let view = fun ty -> TypeRepr.view (TypeRepr.prune ty)

let pattern_binding = fun (state: state) pat_id ~name ~scheme ->
  Binding.make
    ~ident:(fresh_binding_ident state name)
    ~path:(IdentPath.of_name name)
    ~scheme
    ~provenance:(Binding.LoweredPattern pat_id)

let generalized_pattern_binding = fun (state: state) pat_id ~name ty ->
  pattern_binding state pat_id ~name ~scheme:(TypeScheme.of_type ty)

let package_env = fun (state: state) pat_id (signature: TypeRepr.package_signature) ->
  let entries = signature.values
  |> List.map
    (fun (value: TypeRepr.package_value) ->
      pattern_binding state pat_id ~name:value.name ~scheme:value.scheme) in
  Env.of_bindings entries

let env_of_module_scope = fun scope ->
  Env.bind
    (Env.of_bindings (Env.Value_env.bindings (Env.scope_values scope)))
    (Env.of_type_decls (Env.Type_env.type_decls (Env.scope_types scope)))

let type_item_env = fun (_state: state) (type_item: ItemTree.type_item) ->
  Env.of_type_decls
    [ { FileSummary.scope_path = IdentPath.empty; declaration = type_item.declaration } ]

let resolve_named_type_head_in_env = fun (state: state) env name ->
  Env.lookup_type env name
  |> Option.map
    (fun (type_decl: FileSummary.type_decl) ->
      TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name)
  |> fun resolved ->
    Option.or_else resolved
      (fun () ->
        State.resolve_named_type_head state name)

let canonicalize_scheme_in_env = fun (state: state) env scheme ->
  let scheme =
    State.canonicalize_scheme_with_named_type_head
      (fun name -> resolve_named_type_head_in_env state env name)
      scheme
  in
  let visible_types = VisibleTypes.bind state.visible_types (Env.visible_type_decls env) in
  VisibleTypes.canonicalize_scheme visible_types scheme

let canonicalize_scheme_heads_in_env = fun (state: state) env scheme ->
  State.canonicalize_scheme_with_named_type_head
    (fun name -> resolve_named_type_head_in_env state env name)
    scheme

let resolve_named_type_decl_in_env = fun (state: state) env name ->
  Option.or_else (Env.lookup_type env name)
    (fun () ->
      State.visible_type_decl state name)

let canonicalize_type_decl_in_env = fun (state: state) env (type_decl: FileSummary.type_decl) ->
  let current_decl_name = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name in
  let resolve_named_type_head name =
    if not type_decl.declaration.nonrec_ && IdentPath.equal name current_decl_name then
      Some (TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name)
    else
      resolve_named_type_head_in_env state env name
  in
  State.canonicalize_type_decl_with_named_type_head resolve_named_type_head type_decl

let constructor_bindings = fun (state: state) env ~name ~scheme ~provenance ~constructor_id ~inline_record_labels ->
  let visible_types = VisibleTypes.bind state.visible_types (Env.visible_type_decls env) in
  let scheme = canonicalize_scheme_in_env state env scheme in
  let inline_record_labels = inline_record_labels
  |> Option.map (VisibleTypes.canonicalize_inline_record_labels visible_types) in
  let rec owner_result_type ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Arrow { rhs; _ } -> owner_result_type rhs
    | _ -> ty
  in
  let owner_ty = owner_result_type (TypeScheme.body scheme) in
  match TypeRepr.view (TypeRepr.prune owner_ty) with
  | TypeRepr.Named { head; _ } -> Env.singleton_constructor
    ~make_ident:(fresh_binding_ident state)
    ~name
    ~scheme
    ~provenance
    ~owner_path:head.name
    ~owner_type_constructor_id:head.type_constructor_id
    ~constructor_id
    ~inline_record_labels
  | _ -> Env.singleton ~make_ident:(fresh_binding_ident state) ~name ~scheme ~provenance

let exception_bindings = fun (state: state) env (exception_item: ItemTree.exception_item) ->
  constructor_bindings
    state
    env
    ~name:exception_item.exception_name
    ~scheme:exception_item.scheme
    ~provenance:(Binding.Exception {
      name = exception_item.exception_name;
      scope_path = exception_item.scope_path
    })
    ~constructor_id:(ConstructorId.of_path
      (IdentPath.append_name exception_item.scope_path exception_item.exception_name))
    ~inline_record_labels:None

let extension_constructor_bindings = fun (state: state) env (
  extension_item: ItemTree.extension_constructor_item
) ->
  constructor_bindings
    state
    env
    ~name:extension_item.constructor_name
    ~scheme:extension_item.scheme
    ~provenance:(Binding.DeclaredValue {
      name = extension_item.constructor_name;
      scope_path = extension_item.scope_path
    })
    ~constructor_id:extension_item.constructor_id
    ~inline_record_labels:extension_item.inline_record_labels

let declared_value_bindings = fun (state: state) env (
  declared_value_item: ItemTree.declared_value_item
) ->
  Env.singleton
    ~make_ident:(fresh_binding_ident state)
    ~name:declared_value_item.value_name
    ~scheme:(canonicalize_scheme_heads_in_env state env declared_value_item.scheme)
    ~provenance:(Binding.DeclaredValue {
      name = declared_value_item.value_name;
      scope_path = declared_value_item.scope_path
    })

let canonicalize_generalized_scheme_in_env = fun (state: state) env scheme ->
  canonicalize_scheme_heads_in_env state env scheme

let instantiate = fun (state: state) scheme ->
  Solver.instantiate state.solver scheme

let substitute_type_vars = fun (state: state) ty mapping ->
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int ->
        ty
    | TypeRepr.Float ->
        ty
    | TypeRepr.Bool ->
        ty
    | TypeRepr.String ->
        ty
    | TypeRepr.Char ->
        ty
    | TypeRepr.Unit ->
        ty
    | TypeRepr.Option element ->
        let copied_element = loop element in
        if Std.Ptr.equal element copied_element then
          ty
        else
          make_type state (TypeRepr.Option copied_element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        let copied_ok_ty = loop ok_ty in
        let copied_error_ty = loop error_ty in
        if Std.Ptr.equal ok_ty copied_ok_ty && Std.Ptr.equal error_ty copied_error_ty then
          ty
        else
          make_type state (TypeRepr.Result (copied_ok_ty, copied_error_ty))
    | TypeRepr.Array element ->
        let copied_element = loop element in
        if Std.Ptr.equal element copied_element then
          ty
        else
          make_type state (TypeRepr.Array copied_element)
    | TypeRepr.List element ->
        let copied_element = loop element in
        if Std.Ptr.equal element copied_element then
          ty
        else
          make_type state (TypeRepr.List copied_element)
    | TypeRepr.Seq element ->
        let copied_element = loop element in
        if Std.Ptr.equal element copied_element then
          ty
        else
          make_type state (TypeRepr.Seq copied_element)
    | TypeRepr.Named { head; arguments } ->
        let copied_arguments = List.map loop arguments in
        if List.for_all2 Std.Ptr.equal arguments copied_arguments then
          ty
        else
          make_type state (TypeRepr.Named { head; arguments = copied_arguments })
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let copied_tags =
          tags
          |> List.map
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let copied_payload_type = loop payload_type in
                  if Std.Ptr.equal payload_type copied_payload_type then
                    tag
                  else
                    { tag with payload_type = Some copied_payload_type }
              | None -> tag)
        in
        let copied_inherited = List.map loop inherited in
        if
          List.for_all2 Std.Ptr.equal tags copied_tags && List.for_all2 Std.Ptr.equal inherited copied_inherited
        then
          ty
        else
          make_type
            state
            (TypeRepr.PolyVariant { bound; tags = copied_tags; inherited = copied_inherited })
    | TypeRepr.Hole hole_id ->
        make_type state (TypeRepr.Hole hole_id)
    | TypeRepr.Tuple members ->
        let copied_members = List.map loop members in
        if List.for_all2 Std.Ptr.equal members copied_members then
          ty
        else
          make_type state (TypeRepr.Tuple copied_members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let copied_lhs = loop lhs in
        let copied_rhs = loop rhs in
        if Std.Ptr.equal lhs copied_lhs && Std.Ptr.equal rhs copied_rhs then
          ty
        else
          make_type state (TypeRepr.Arrow { label; lhs = copied_lhs; rhs = copied_rhs })
    | TypeRepr.Package signature ->
        let copied_values =
          signature.values
          |> List.map
            (fun (value: TypeRepr.package_value) ->
              let copied_scheme = TypeScheme.map_type_preserving loop value.scheme in
              if Std.Ptr.equal value.scheme copied_scheme then
                value
              else
                  { value with scheme = copied_scheme })
        in
        if List.for_all2 Std.Ptr.equal signature.values copied_values then
          ty
        else
          TypeRepr.package ~values:copied_values
    | TypeRepr.Var var -> (
        match var.link with
        | Some linked -> loop linked
        | None ->
            match Collections.HashMap.get mapping var.id with
            | Some replacement -> replacement
            | None -> ty
      )
  in
  loop ty

let normalize_rigid_type = fun (state: state) ->
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
        let normalized_element = loop element in
        if Std.Ptr.equal element normalized_element then
          ty
        else
          make_type state (TypeRepr.Option normalized_element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        let normalized_ok_ty = loop ok_ty in
        let normalized_error_ty = loop error_ty in
        if Std.Ptr.equal ok_ty normalized_ok_ty && Std.Ptr.equal error_ty normalized_error_ty then
          ty
        else
          make_type state (TypeRepr.Result (normalized_ok_ty, normalized_error_ty))
    | TypeRepr.Array element ->
        let normalized_element = loop element in
        if Std.Ptr.equal element normalized_element then
          ty
        else
          make_type state (TypeRepr.Array normalized_element)
    | TypeRepr.List element ->
        let normalized_element = loop element in
        if Std.Ptr.equal element normalized_element then
          ty
        else
          make_type state (TypeRepr.List normalized_element)
    | TypeRepr.Seq element ->
        let normalized_element = loop element in
        if Std.Ptr.equal element normalized_element then
          ty
        else
          make_type state (TypeRepr.Seq normalized_element)
    | TypeRepr.Named { head; arguments } ->
        let normalized_arguments = List.map loop arguments in
        if List.for_all2 Std.Ptr.equal arguments normalized_arguments then
          ty
        else
          make_type state (TypeRepr.Named { head; arguments = normalized_arguments })
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let normalized_tags =
          tags
          |> List.map
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let normalized_payload_type = loop payload_type in
                  if Std.Ptr.equal payload_type normalized_payload_type then
                    tag
                  else
                    { tag with payload_type = Some normalized_payload_type }
              | None -> tag)
        in
        let normalized_inherited = List.map loop inherited in
        if
          List.for_all2 Std.Ptr.equal tags normalized_tags
          && List.for_all2 Std.Ptr.equal inherited normalized_inherited
        then
          ty
        else
          make_type
            state
            (TypeRepr.PolyVariant { bound; tags = normalized_tags; inherited = normalized_inherited })
    | TypeRepr.Tuple members ->
        let normalized_members = List.map loop members in
        if List.for_all2 Std.Ptr.equal members normalized_members then
          ty
        else
          make_type state (TypeRepr.Tuple normalized_members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let normalized_lhs = loop lhs in
        let normalized_rhs = loop rhs in
        if Std.Ptr.equal lhs normalized_lhs && Std.Ptr.equal rhs normalized_rhs then
          ty
        else
          make_type state (TypeRepr.Arrow { label; lhs = normalized_lhs; rhs = normalized_rhs })
    | TypeRepr.Package signature ->
        let normalized_values =
          signature.values
          |> List.map
            (fun (value: TypeRepr.package_value) ->
              let normalized_scheme = TypeScheme.map_type_preserving loop value.scheme in
              if Std.Ptr.equal value.scheme normalized_scheme then
                value
              else
                { value with scheme = normalized_scheme })
        in
        if List.for_all2 Std.Ptr.equal signature.values normalized_values then
          ty
        else
          TypeRepr.package ~values:normalized_values
    | TypeRepr.Var { id; kind = TypeRepr.Rigid; _ } -> (
        match State.lookup_rigid_equation state id with
        | Some replacement -> loop replacement
        | None -> ty
      )
    | TypeRepr.Var _ ->
        ty
  in
  loop

let instantiate_rigid_scheme = fun (state: state) scheme ->
  TypeScheme.instantiate
    ~fresh_var:(fun () -> fresh_rigid_var state)
    ~make:(make_type state)
    ~next_mark:(fun () -> Solver.next_mark state.solver)
    scheme

let canonicalize_type_in_env = fun (state: state) env ty ->
  let canonicalized =
    State.canonicalize_type_with_name_resolution
      ~resolve_named_type_decl:(resolve_named_type_decl_in_env state env)
      ~resolve_named_type_head:(fun name -> resolve_named_type_head_in_env state env name)
      ty
  in
  let free_vars = TypeRepr.free_vars canonicalized in
  if List.is_empty free_vars then
    canonicalized
  else
    let mapping = Collections.HashMap.with_capacity (List.length free_vars) in
    let () =
      free_vars
      |> List.iter
        (fun id ->
          let _ = Collections.HashMap.insert mapping id (fresh_var state) in
          ())
    in
    substitute_type_vars state canonicalized mapping

let labels_match = fun left right ->
  match (left, right) with
  | (TypeRepr.Nolabel, TypeRepr.Nolabel) -> true
  | (TypeRepr.Labelled left, TypeRepr.Labelled right) -> String.equal left right
  | (TypeRepr.Optional left, TypeRepr.Optional right) -> String.equal left right
  | _ -> false

let type_label_of_body_label = function
  | BodyArena.Positional -> TypeRepr.Nolabel
  | BodyArena.Labeled label -> TypeRepr.Labelled label
  | BodyArena.Optional label -> TypeRepr.Optional label

let diagnostic_label_of_type_label = function
  | TypeRepr.Nolabel -> Typ_diagnostic.PositionalArgument
  | TypeRepr.Labelled label -> Typ_diagnostic.LabeledArgument label
  | TypeRepr.Optional label -> Typ_diagnostic.OptionalArgument label

let diagnostic_label_of_body_label = function
  | BodyArena.Positional -> Typ_diagnostic.PositionalArgument
  | BodyArena.Labeled label -> Typ_diagnostic.LabeledArgument label
  | BodyArena.Optional label -> Typ_diagnostic.OptionalArgument label

let argument_matches_parameter_label = fun parameter_label argument_label ->
  match (parameter_label, argument_label) with
  | (TypeRepr.Nolabel, BodyArena.Positional) -> true
  | (TypeRepr.Labelled expected, BodyArena.Labeled actual) -> String.equal expected actual
  | (TypeRepr.Optional expected, BodyArena.Optional actual)
  | (TypeRepr.Optional expected, BodyArena.Labeled actual) -> String.equal expected actual
  | _ -> false

let take_matching_argument = fun parameter_label arguments ->
  let rec loop prefix = function
    | [] -> None
    | ((argument: BodyArena.apply_argument) as candidate) :: rest ->
        if argument_matches_parameter_label parameter_label argument.label then
          Some (candidate, List.rev_append prefix rest)
        else
          loop (candidate :: prefix) rest
  in
  loop [] arguments

let visible_type_decl = fun (state: state) name ->
  State.visible_type_decl state name

let visible_type_decl_by_id = fun (state: state) type_constructor_id ->
  State.visible_type_decl_by_id state type_constructor_id

type poly_variant_candidate = {
  type_decl: FileSummary.type_decl;
  bound: TypeDecl.poly_variant_bound;
  payloads: TypeRepr.t option Label_name_map.t;
}

let owner_of_type = fun (state: state) ty ->
  match view (TypeRepr.prune ty) with
  | TypeRepr.Named { head={ type_constructor_id; _ }; _ } -> Some { type_constructor_id }
  | _ -> None

let rec result_type_of_type = fun ty ->
  let ty = TypeRepr.prune ty in
  match TypeRepr.view ty with
  | TypeRepr.Arrow { rhs; _ } -> result_type_of_type rhs
  | _ -> ty

let resolve_constructor_entry = fun (state: state) env constructor ~expected_ty ->
  let candidates = Env.lookup_constructors env constructor in
  match owner_of_type state expected_ty with
  | Some expected_owner -> (
      match Env.lookup_owned_constructor env constructor expected_owner.type_constructor_id with
      | Some candidate -> Some candidate
      | None -> (
          match
            List.filter
              (fun candidate ->
                TypeConstructorId.equal
                  expected_owner.type_constructor_id
                  (Env.Constructor_env.owner_type_constructor_id candidate))
              candidates
          with
          | candidate :: _ -> Some candidate
          | [] -> (
              match candidates with
              | candidate :: _ -> Some candidate
              | [] -> None
            )
        )
    )
  | None -> (
      match candidates with
      | candidate :: _ -> Some candidate
      | [] -> None
    )

let resolve_constructor_without_expected = fun env constructor ->
  Env.lookup_constructors env constructor |> function
  | candidate :: _ -> Some candidate
  | [] -> None

let record_decl_matches_owner = fun (record_decl: record_type_decl) owner ->
  TypeConstructorId.equal (Env.Label_env.owner_type_constructor_id record_decl) owner.type_constructor_id

let explicit_record_field_module_path = fun field_name ->
  match IdentPath.split_last (IdentPath.of_string field_name) with
  | Some (module_path, _label_name) when not (IdentPath.is_empty module_path) -> Some module_path
  | _ -> None

let record_decl_matches_explicit_field_owners = fun (record_decl: record_type_decl) field_names ->
  let owner_module_path =
    match IdentPath.split_last (Env.Label_env.owner_path record_decl) with
    | Some (module_path, _type_name) -> module_path
    | None -> IdentPath.empty
  in
  List.for_all
    (fun field_name ->
      match explicit_record_field_module_path field_name with
      | Some module_path -> IdentPath.equal module_path owner_module_path
      | None -> true)
    field_names

let instantiate_record_decl = fun (state: state) (record_decl: record_type_decl) ->
  let mapping = Collections.HashMap.with_capacity 8 in
  let () =
    Env.Label_env.param_ids record_decl
    |> List.iter
      (fun quantified_id ->
        let _ = Collections.HashMap.insert mapping quantified_id (fresh_var state) in
        ())
  in
  let owner_path =
    match visible_type_decl_by_id state (Env.Label_env.owner_type_constructor_id record_decl) with
    | Some type_decl -> qualify_name type_decl.scope_path type_decl.declaration.type_name
    | None -> Env.Label_env.owner_path record_decl
  in
  let owner_ty = make_type state
    (
      TypeRepr.Named {
        head = TypeRepr.named_head
          ~type_constructor_id:(Env.Label_env.owner_type_constructor_id record_decl)
          ~name:owner_path;
        arguments =
          Env.Label_env.param_ids record_decl |> List.map
            (fun quantified_id ->
              match Collections.HashMap.get mapping quantified_id with
              | Some ty -> ty
              | None -> make_type state (TypeRepr.Hole missing_record_decl_param_hole_id));
      }
    )
  in
  let field_types =
    Env.Label_env.labels record_decl
    |> List.fold_left
      (fun acc (field: TypeDecl.label) ->
        let field_scheme =
          TypeScheme.map_type_preserving
            (fun ty -> substitute_type_vars state ty mapping)
            field.field_type
        in
        Label_name_map.add (Env.Label_env.lookup_name field.name) field_scheme acc)
      Label_name_map.empty
  in
  (owner_ty, field_types)

let record_field_type = fun field_types label_name ->
  Label_name_map.find_opt (Env.Label_env.lookup_name label_name) field_types

let type_decl_path = fun (type_decl: FileSummary.type_decl) ->
  qualify_name type_decl.scope_path type_decl.declaration.type_name

let instantiate_named_type_decl = fun (state: state) (type_decl: FileSummary.type_decl) ->
  let mapping = Collections.HashMap.with_capacity 8 in
  let arguments =
    type_decl.declaration.param_ids
    |> List.map
      (fun param_id ->
        let argument = fresh_var state in
        let _ = Collections.HashMap.insert mapping param_id argument in
        argument)
  in
  (
    make_type
      state
      (TypeRepr.Named {
        head = TypeRepr.named_head
          ~type_constructor_id:type_decl.declaration.type_constructor_id
          ~name:(type_decl_path type_decl);
        arguments
      }),
    mapping
  )

let instantiate_poly_variant_payload = fun (state: state) mapping (payload_type: TypeRepr.t option) ->
  payload_type |> Option.map (fun payload_type -> substitute_type_vars state payload_type mapping)

let poly_variant_candidate = fun (state: state) (type_decl: FileSummary.type_decl) ->
  let rec collect visited acc (type_decl: FileSummary.type_decl) =
    let type_constructor_id = type_decl.declaration.type_constructor_id in
    if Collections.HashSet.contains visited type_constructor_id then
      acc
    else
      let () = Collections.HashSet.insert visited type_constructor_id |> ignore in
      match type_decl.declaration.manifest with
      | Some (TypeDecl.PolyVariant { tags; inherited; _ }) ->
          let acc =
            tags
            |> List.fold_left
              (fun acc (tag: TypeDecl.poly_variant_tag) ->
                Label_name_map.add tag.name tag.payload_type acc)
              acc
          in
          inherited |> List.fold_left
            (fun acc inherited_type ->
              match view inherited_type with
              | TypeRepr.Named { head={ type_constructor_id; _ }; _ } -> (
                  match visible_type_decl_by_id state type_constructor_id with
                  | Some inherited_decl -> collect visited acc inherited_decl
                  | None -> acc
                )
              | _ -> acc)
            acc
      | _ -> acc
  in
  match type_decl.declaration.manifest with
  | Some (TypeDecl.PolyVariant { bound; _ }) -> Some {
    type_decl;
    bound;
    payloads = collect (Collections.HashSet.create ()) Label_name_map.empty type_decl
  }
  | _ -> None

let poly_variant_tag_count = fun candidate -> candidate.payloads |> Label_name_map.bindings |> List.length

let poly_variant_candidate_payload = fun candidate tag ->
  match Label_name_map.find_opt tag candidate.payloads with
  | Some payload_type -> payload_type
  | None -> None

let compare_poly_variant_candidates = fun left right ->
  match Int.compare (poly_variant_tag_count left) (poly_variant_tag_count right) with
  | 0 -> IdentPath.compare (type_decl_path left.type_decl) (type_decl_path right.type_decl)
  | order -> order

let best_poly_variant_candidate_for_tags = fun (state: state) tags ->
  visible_type_decls state |> List.filter_map (poly_variant_candidate state) |> List.filter
    (fun candidate ->
      tags |> List.for_all
        (fun tag ->
          Label_name_map.mem tag candidate.payloads)) |> List.sort compare_poly_variant_candidates |> function
  | candidate :: _ -> Some candidate
  | [] -> None

let dedupe_poly_variant_tags = fun tags ->
  let seen = Collections.HashSet.create () in
  tags |> List.filter
    (fun tag ->
      if Collections.HashSet.contains seen tag then
        false
      else
        let () = Collections.HashSet.insert seen tag |> ignore in
        true)

let poly_variant_match_score = fun candidate tags ->
  tags |> List.fold_left
    (fun count tag ->
      if Label_name_map.mem tag candidate.payloads then
        count + 1
      else
        count)
    0

let best_poly_variant_match_candidate_for_tags = fun (state: state) tags ->
  let tags = dedupe_poly_variant_tags tags in
  visible_type_decls state |> List.filter_map (poly_variant_candidate state) |> List.fold_left
    (fun best candidate ->
      let candidate_score = poly_variant_match_score candidate tags in
      if candidate_score <= 0 then
        best
      else
        match best with
        | None -> Some (candidate, candidate_score)
        | Some (best_candidate, best_score) ->
            if candidate_score > best_score then
              Some (candidate, candidate_score)
            else if candidate_score < best_score then
              best
            else if compare_poly_variant_candidates candidate best_candidate < 0 then
              Some (candidate, candidate_score)
            else
              best)
    None |> Option.map fst

let poly_variant_candidate_for_type = fun (state: state) ty ->
  match view (canonicalize_type state ty) with
  | TypeRepr.Named { head={ type_constructor_id; _ }; _ } -> Option.and_then
    (visible_type_decl_by_id state type_constructor_id)
    (poly_variant_candidate state)
  | _ -> None

let poly_variant_payloads_compatible = fun (state: state) left right ->
  match (left, right) with
  | (None, None) -> true
  | (Some left, Some right) -> String.equal
    (TypePrinter.type_to_string (canonicalize_type state left))
    (TypePrinter.type_to_string (canonicalize_type state right))
  | _ -> false

let can_explicitly_coerce_poly_variant = fun (state: state) ~source_ty ~target_ty ->
  match (
    poly_variant_candidate_for_type state source_ty,
    poly_variant_candidate_for_type state target_ty
  ) with
  | (Some source, Some target) when source.bound = TypeDecl.Exact ->
      source.payloads |> Label_name_map.bindings |> List.for_all
        (fun (tag, source_payload) ->
          match Label_name_map.find_opt tag target.payloads with
          | Some target_payload -> poly_variant_payloads_compatible state source_payload target_payload
          | None -> false)
  | _ -> false

let anonymous_poly_variant_type = fun (state: state) tag payload_type ->
  make_type
    state
    (TypeRepr.PolyVariant {
      bound = TypeRepr.UpperBound;
      tags = [ TypeRepr.poly_variant_tag ?payload_type tag ];
      inherited = []
    })

let poly_variant_payload_for_expected_type = fun (state: state) expected_ty tag ->
  match view expected_ty with
  | TypeRepr.Named { head={ type_constructor_id; _ }; _ } ->
      Option.and_then (visible_type_decl_by_id state type_constructor_id)
        (fun type_decl ->
          Option.and_then
            (poly_variant_candidate state type_decl)
            (fun candidate -> Label_name_map.find_opt tag candidate.payloads))
  | TypeRepr.PolyVariant { tags; _ } ->
      let rec loop = function
        | [] -> None
        | (candidate: TypeRepr.poly_variant_tag) :: rest ->
            if String.equal candidate.name tag then
              Some candidate.payload_type
            else
              loop rest
      in
      loop tags
  | _ -> None

let is_unresolved_type = fun ty ->
  match view ty with
  | TypeRepr.Var _
  | TypeRepr.Hole _ -> true
  | _ -> false

let rec top_level_poly_variant_tag = fun (state: state) pat_id ->
  match SemanticTree.find_pattern state.file pat_id with
  | Some { desc=BodyArena.PPolyVariant { tag; _ }; _ } -> Some tag
  | Some { desc=BodyArena.PAlias { pattern_id; _ }; _ } -> top_level_poly_variant_tag state pattern_id
  | _ -> None

let constrain_match_scrutinee_to_poly_variant = fun (state: state) scrutinee_ty cases ->
  let tags = cases
  |> List.filter_map
    (fun (case: BodyArena.match_case) -> top_level_poly_variant_tag state case.pattern_id) in
  if List.is_empty tags || not (List.length tags = List.length cases) then
    ()
  else
    match best_poly_variant_match_candidate_for_tags state tags with
    | Some candidate ->
        let (candidate_ty, _mapping) = instantiate_named_type_decl state candidate.type_decl in
        (
          match Solver.unify state.solver ~left:scrutinee_ty ~right:candidate_ty with
          | Ok () -> ()
          | Error _ -> ()
        )
    | None -> ()

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let add_record_resolution_error = fun (state: state) ~span ~context reason ->
  add_diagnostic
    state
    (Typ_diagnostic.RecordResolutionError { operation_span = span; context; reason })

let add_or_pattern_bindings_mismatch = fun (state: state) ~span ~expected_names ~actual_names ->
  add_diagnostic
    state
    (Typ_diagnostic.OrPatternBindingsMismatch { pattern_span = span; expected_names; actual_names })

let resolve_record_decl = fun env (state: state) ~field_names ~owner_hint ~span ~context ->
  let name_candidates =
    match field_names with
    | first_field :: remaining_fields ->
        Env.lookup_record_decls env first_field |> List.filter
          (fun (record_decl: record_type_decl) ->
            Env.Label_env.matches_fields record_decl remaining_fields)
    | [] -> Env.record_decls env
  in
  let candidates =
    match owner_hint with
    | Some owner -> (
        match Env.lookup_record_decl_by_owner env owner.type_constructor_id with
        | Some record_decl when Env.Label_env.matches_fields record_decl field_names -> [
          record_decl
        ]
        | _ -> name_candidates
        |> List.filter
          (fun (record_decl: record_type_decl) -> record_decl_matches_owner record_decl owner)
      )
    | None -> name_candidates
  in
  let candidates = candidates
  |> List.filter
    (fun (record_decl: record_type_decl) -> record_decl_matches_explicit_field_owners record_decl field_names) in
  match candidates with
  | [ record_decl ] ->
      Some record_decl
  | [] ->
      let unknown_labels = field_names
      |> List.filter (fun field_name -> List.is_empty (Env.lookup_record_decls env field_name)) in
      let reason =
        if not (List.is_empty unknown_labels) then
          Typ_diagnostic.UnknownRecordLabels unknown_labels
        else
          Typ_diagnostic.IncompatibleRecordLabels field_names
      in
      let () = add_record_resolution_error state ~span ~context reason in
      None
  | _ ->
      let () = add_record_resolution_error
        state
        ~span
        ~context
        (Typ_diagnostic.AmbiguousRecordLabels field_names) in
      None

let constructor_payload_types = fun (constructor: TypeDecl.constructor) ->
  let rec loop acc ty =
    match view ty with
    | TypeRepr.Arrow { lhs; rhs; _ } -> loop (lhs :: acc) rhs
    | _ -> List.rev acc
  in
  loop [] (TypeScheme.body constructor.scheme)

type coverage_constructor_key =
  | CoverageBool of bool
  | CoverageUnit
  | CoverageTuple of int
  | CoverageOptionNone
  | CoverageOptionSome
  | CoverageResultOk
  | CoverageResultError
  | CoverageListNil
  | CoverageListCons
  | CoverageVariant of ConstructorId.t

type coverage_constructor = {
  key: coverage_constructor_key;
  name: string;
  argument_types: TypeRepr.t list;
}

type coverage_pattern =
  | CoverageAny
  | CoverageConstructor of { constructor: coverage_constructor; arguments: coverage_pattern list }

type coverage_row = coverage_pattern list

let coverage_constructor_key_equal = fun left right ->
  match (left, right) with
  | (CoverageBool left, CoverageBool right) -> Bool.equal left right
  | (CoverageUnit, CoverageUnit) -> true
  | (CoverageTuple left, CoverageTuple right) -> Int.equal left right
  | (CoverageOptionNone, CoverageOptionNone) -> true
  | (CoverageOptionSome, CoverageOptionSome) -> true
  | (CoverageResultOk, CoverageResultOk) -> true
  | (CoverageResultError, CoverageResultError) -> true
  | (CoverageListNil, CoverageListNil) -> true
  | (CoverageListCons, CoverageListCons) -> true
  | (CoverageVariant left, CoverageVariant right) -> ConstructorId.equal left right
  | _ -> false

let make_coverage_constructor = fun ~key ~name ~argument_types -> { key; name; argument_types }

let coverage_constructor_arity = fun constructor -> List.length constructor.argument_types

let rec decompose_arrow_type = fun ty ->
  match view ty with
  | TypeRepr.Arrow { lhs; rhs; _ } ->
      let (arguments, result_ty) = decompose_arrow_type rhs in
      (lhs :: arguments, result_ty)
  | _ -> ([], ty)

type instantiated_constructor_entry = {
  argument_types: TypeRepr.t list;
  result_ty: TypeRepr.t;
  generalized: bool;
  inline_record_labels: TypeDecl.label list option;
}

let instantiate_constructor_entry = fun ?(pattern_mode = false) (state: state) env constructor_entry ->
  let scheme = canonicalize_scheme_in_env state env (Env.Constructor_env.scheme constructor_entry) in
  let quantified, body = TypeScheme.to_explicit scheme in
  let mapping = Collections.HashMap.with_capacity (List.length quantified) in
  let () =
    quantified
    |> List.iter
      (fun quantified_id ->
        let replacement =
          if pattern_mode && Env.Constructor_env.generalized constructor_entry then
            fresh_rigid_var state
          else
            fresh_var state
        in
        let _ = Collections.HashMap.insert mapping quantified_id replacement in
        ())
  in
  let body = substitute_type_vars state body mapping in
  let inline_record_labels =
    Env.Constructor_env.inline_record_labels constructor_entry
    |> Option.map
      (
        List.map
          (fun (label: TypeDecl.label) ->
            let field_type =
              TypeScheme.map_type_preserving
                (fun ty -> substitute_type_vars state ty mapping)
                label.field_type
            in
            if Std.Ptr.equal label.field_type field_type then
              label
            else
              { label with field_type })
      )
  in
  let (argument_types, result_ty) = decompose_arrow_type body in
  {
    argument_types;
    result_ty;
    generalized = Env.Constructor_env.generalized constructor_entry;
    inline_record_labels
  }

let field_types_of_labels = fun (state: state) labels ->
  labels |> List.fold_left
    (fun acc (label: TypeDecl.label) ->
      Label_name_map.add
        (Env.Label_env.lookup_name label.name)
        label.field_type
        acc)
    Label_name_map.empty

let missing_inline_record_fields = fun labels field_names ->
  labels |> List.filter_map
    (fun (label: TypeDecl.label) ->
      if List.exists
          (fun requested_name ->
            String.equal (Env.Label_env.lookup_name requested_name) label.name)
          field_names then
        None
      else
        Some label.name)

let pattern_name = fun path ->
  IdentPath.last_name path |> Option.unwrap_or ~default:(IdentPath.to_string path)

let coverage_witness_of_constructor = fun (constructor: coverage_constructor) arguments ->
  match constructor.key with
  | CoverageBool value -> Typ_diagnostic.BoolWitness value
  | CoverageUnit -> Typ_diagnostic.UnitWitness
  | CoverageTuple _ -> Typ_diagnostic.TupleWitness arguments
  | CoverageOptionNone
  | CoverageOptionSome
  | CoverageResultOk
  | CoverageResultError
  | CoverageListNil
  | CoverageListCons
  | CoverageVariant _ -> Typ_diagnostic.ConstructorWitness { name = constructor.name; arguments }

let rec coverage_product = function
  | [] -> [ [] ]
  | patterns :: rest ->
      let rest = coverage_product rest in
      patterns |> List.concat_map (fun pattern -> rest |> List.map (fun suffix -> pattern :: suffix))

let supported_coverage_constructors_for_named_type = fun (state: state) head arguments ->
  match visible_type_decl_by_id state head.TypeRepr.type_constructor_id with
  | Some type_decl when List.is_empty type_decl.declaration.labels
  && not (List.is_empty type_decl.declaration.constructors) ->
      let mapping = Collections.HashMap.with_capacity 8 in
      let () =
        List.iter2
          (fun param_id argument ->
            let _ = Collections.HashMap.insert mapping param_id argument in
            ())
          type_decl.declaration.param_ids
          arguments
      in
      Some (
        type_decl.declaration.constructors |> List.map
          (fun (constructor: TypeDecl.constructor) ->
            let specialized = substitute_type_vars state (TypeScheme.body constructor.scheme) mapping in
            let (argument_types, _result_ty) = decompose_arrow_type specialized in
            make_coverage_constructor
              ~key:(CoverageVariant constructor.constructor_id)
              ~name:constructor.name
              ~argument_types)
      )
  | _ -> None

let supported_coverage_constructors_for_type = fun (state: state) ty ->
  match view ty with
  | TypeRepr.Bool -> Some [
    make_coverage_constructor ~key:(CoverageBool false) ~name:"false" ~argument_types:[];
    make_coverage_constructor ~key:(CoverageBool true) ~name:"true" ~argument_types:[];
  ]
  | TypeRepr.Unit -> Some [
    make_coverage_constructor ~key:CoverageUnit ~name:"()" ~argument_types:[]
  ]
  | TypeRepr.Tuple element_types -> Some [
    make_coverage_constructor
      ~key:(CoverageTuple (List.length element_types))
      ~name:"tuple"
      ~argument_types:element_types
  ]
  | TypeRepr.Option element_ty -> Some [
    make_coverage_constructor ~key:CoverageOptionNone ~name:"None" ~argument_types:[];
    make_coverage_constructor ~key:CoverageOptionSome ~name:"Some" ~argument_types:[ element_ty ];
  ]
  | TypeRepr.Result (ok_ty, error_ty) -> Some [
    make_coverage_constructor ~key:CoverageResultOk ~name:"Ok" ~argument_types:[ ok_ty ];
    make_coverage_constructor ~key:CoverageResultError ~name:"Error" ~argument_types:[ error_ty ];
  ]
  | TypeRepr.List element_ty -> Some [
    make_coverage_constructor ~key:CoverageListNil ~name:"[]" ~argument_types:[];
    make_coverage_constructor ~key:CoverageListCons ~name:"::" ~argument_types:[ element_ty; ty ];
  ]
  | TypeRepr.Named { head; arguments } -> supported_coverage_constructors_for_named_type state head arguments
  | TypeRepr.Int
  | TypeRepr.Float
  | TypeRepr.String
  | TypeRepr.Char
  | TypeRepr.Array _
  | TypeRepr.Seq _
  | TypeRepr.Package _
  | TypeRepr.PolyVariant _
  | TypeRepr.Arrow _
  | TypeRepr.Var _
  | TypeRepr.Hole _ -> None

let supported_constructor_for_pattern = fun (state: state) expected_ty constructor_name argument_count ->
  Option.and_then
    (supported_coverage_constructors_for_type state expected_ty)
    (fun constructors ->
      constructors
      |> List.find_opt
        (fun constructor ->
          String.equal constructor.name constructor_name
          && Int.equal (coverage_constructor_arity constructor) argument_count))

let rec coverage_patterns_of_pattern = fun (state: state) pat_id expected_ty ->
  let combine_with_constructor constructor pattern_groups = coverage_product pattern_groups
  |> List.map (fun arguments -> CoverageConstructor { constructor; arguments }) in
  let translate_many pattern_ids expected_types =
    let rec loop acc pattern_ids expected_types =
      match (pattern_ids, expected_types) with
      | ([], []) ->
          Some (List.rev acc)
      | (pattern_id :: rest_patterns, expected_ty :: rest_types) -> (
          match coverage_patterns_of_pattern state pattern_id expected_ty with
          | Some patterns -> loop (patterns :: acc) rest_patterns rest_types
          | None -> None
        )
      | _ ->
          None
    in
    loop [] pattern_ids expected_types
  in
  match SemanticTree.find_pattern state.file pat_id with
  | None -> None
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar _
      | BodyArena.PWildcard ->
          Some [ CoverageAny ]
      | BodyArena.PBool value -> (
          match supported_constructor_for_pattern state expected_ty (Bool.to_string value) 0 with
          | Some constructor -> Some [ CoverageConstructor { constructor; arguments = [] } ]
          | None -> None
        )
      | BodyArena.PUnit -> (
          match supported_constructor_for_pattern state expected_ty "()" 0 with
          | Some constructor -> Some [ CoverageConstructor { constructor; arguments = [] } ]
          | None -> None
        )
      | BodyArena.PTuple elements -> (
          match supported_coverage_constructors_for_type state expected_ty with
          | Some [ constructor ] when (
            match constructor.key with
            | CoverageTuple _ -> true
            | _ -> false
          ) -> translate_many elements constructor.argument_types
          |> Option.map (combine_with_constructor constructor)
          | _ -> None
        )
      | BodyArena.POr alternatives ->
          let rec loop acc = function
            | [] -> Some (List.rev acc)
            | alternative_id :: rest -> (
                match coverage_patterns_of_pattern state alternative_id expected_ty with
                | Some patterns -> loop (List.rev_append patterns acc) rest
                | None -> None
              )
          in
          loop [] alternatives
      | BodyArena.PConstructor { constructor; arguments } -> (
          match supported_constructor_for_pattern
            state
            expected_ty
            (pattern_name constructor)
            (List.length arguments) with
          | Some constructor -> translate_many arguments constructor.argument_types
          |> Option.map (combine_with_constructor constructor)
          | None -> None
        )
      | BodyArena.PList elements -> (
          match view expected_ty with
          | TypeRepr.List element_ty ->
              let nil_constructor = make_coverage_constructor
                ~key:CoverageListNil
                ~name:"[]"
                ~argument_types:[] in
              let cons_constructor = make_coverage_constructor
                ~key:CoverageListCons
                ~name:"::"
                ~argument_types:[ element_ty; expected_ty ] in
              let rec loop = function
                | [] -> Some [
                  CoverageConstructor { constructor = nil_constructor; arguments = [] }
                ]
                | element_id :: rest -> (
                    match (coverage_patterns_of_pattern state element_id element_ty, loop rest) with
                    | (Some element_patterns, Some rest_patterns) -> Some (element_patterns
                    |> List.concat_map
                      (fun head ->
                        rest_patterns
                        |> List.map
                          (fun tail ->
                            CoverageConstructor {
                              constructor = cons_constructor;
                              arguments = [ head; tail ]
                            })))
                    | _ -> None
                  )
              in
              loop elements
          | _ -> None
        )
      | BodyArena.PAlias { pattern_id; _ } ->
          coverage_patterns_of_pattern state pattern_id expected_ty
      | BodyArena.PInt _
      | BodyArena.PFloat _
      | BodyArena.PString _
      | BodyArena.PChar _
      | BodyArena.PRecord _
      | BodyArena.PFirstClassModule _
      | BodyArena.PPolyVariant _
      | BodyArena.PUnsupported _ ->
          None
    )

let coverage_any_patterns = fun count ->
  List.init count (fun _ -> CoverageAny)

let specialize_coverage_row = fun constructor (row: coverage_row) ->
  match row with
  | [] -> None
  | CoverageAny :: rest -> Some (coverage_any_patterns (coverage_constructor_arity constructor) @ rest)
  | CoverageConstructor { constructor=candidate; arguments } :: rest ->
      if coverage_constructor_key_equal constructor.key candidate.key then
        Some (arguments @ rest)
      else
        None

let specialize_coverage_matrix = fun constructor rows ->
  rows |> List.filter_map (specialize_coverage_row constructor)

let default_coverage_matrix = fun rows ->
  rows |> List.filter_map
    (
      function
      | CoverageAny :: rest -> Some rest
      | _ -> None
    )

let split_prefix = fun count values ->
  let rec loop index prefix remaining =
    if Int.equal index 0 then
      (List.rev prefix, remaining)
    else
      match remaining with
      | [] -> (List.rev prefix, [])
      | value :: rest -> loop (index - 1) (value :: prefix) rest
  in
  loop count [] values

let coverage_row_is_all_any = fun row ->
  row |> List.for_all
    (
      function
      | CoverageAny -> true
      | CoverageConstructor _ -> false
    )

let rec useful_coverage_vector = fun (state: state) rows patterns expected_types ->
  match (patterns, expected_types) with
  | ([], []) ->
      if List.exists List.is_empty rows then
        None
      else
        Some []
  | (_ :: _, _) when (rows
  |> List.exists (fun row -> List.length row = List.length patterns && coverage_row_is_all_any row)) ->
      None
  | (CoverageAny :: pattern_rest, _ :: type_rest) when (
    rows |> List.for_all
      (
        function
        | CoverageAny :: _ -> true
        | _ -> false
      )
  ) ->
      useful_coverage_vector state (default_coverage_matrix rows) pattern_rest type_rest
      |> Option.map (fun witness -> Typ_diagnostic.WildcardWitness :: witness)
  | (CoverageAny :: pattern_rest, expected_ty :: type_rest) -> (
      match supported_coverage_constructors_for_type state expected_ty with
      | Some constructors ->
          constructors |> List.find_map
            (fun constructor ->
              let arity = coverage_constructor_arity constructor in
              let specialized_rows = specialize_coverage_matrix constructor rows in
              let specialized_patterns = coverage_any_patterns arity @ pattern_rest in
              let specialized_types = constructor.argument_types @ type_rest in
              useful_coverage_vector state specialized_rows specialized_patterns specialized_types
              |> Option.map
                (fun witness ->
                  let (arguments, tail) = split_prefix arity witness in
                  coverage_witness_of_constructor constructor arguments :: tail))
      | None -> useful_coverage_vector state (default_coverage_matrix rows) pattern_rest type_rest
      |> Option.map (fun witness -> Typ_diagnostic.WildcardWitness :: witness)
    )
  | (CoverageConstructor { constructor; arguments } :: pattern_rest, expected_ty :: type_rest) ->
      ignore expected_ty;
      let specialized_rows = specialize_coverage_matrix constructor rows in
      let specialized_patterns = arguments @ pattern_rest in
      let specialized_types = constructor.argument_types @ type_rest in
      useful_coverage_vector state specialized_rows specialized_patterns specialized_types
      |> Option.map
        (fun witness ->
          let (arguments, tail) = split_prefix (coverage_constructor_arity constructor) witness in
          coverage_witness_of_constructor constructor arguments :: tail)
  | _ ->
      None

let useful_coverage_pattern = fun (state: state) rows pattern expected_ty ->
  Option.and_then (useful_coverage_vector state rows [ pattern ] [ expected_ty ])
    (
      function
      | [ witness ] -> Some witness
      | _ -> None
    )

let origin_label_of_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> None
  | Some expr -> (
      match SemanticTree.find_origin state.file expr.origin_id with
      | Some origin -> Some origin.label
      | None -> None
    )

let rec is_nonexpansive_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> false
  | Some expr ->
      match expr.desc with
      | BodyArena.EVar _
      | BodyArena.EInt _
      | BodyArena.EFloat _
      | BodyArena.EBool _
      | BodyArena.EString _
      | BodyArena.EChar _
      | BodyArena.EUnit ->
          true
      | BodyArena.ETuple element_ids ->
          List.for_all (is_nonexpansive_expr state) element_ids
      | BodyArena.EFun _ ->
          true
      | BodyArena.EModulePack _
      | BodyArena.ELocalModulePack _ ->
          true
      | BodyArena.EApply (callee_id, arguments) ->
          let arguments_nonexpansive = arguments
          |> List.for_all
            (fun (argument: BodyArena.apply_argument) -> is_nonexpansive_expr state argument.value_id) in
          if not arguments_nonexpansive then
            false
          else
            (
              match origin_label_of_expr state expr_id with
              | Some "constructor_apply_expression"
              | Some "list_literal_apply" ->
                  true
              | Some "infix_expression" -> (
                  match SemanticTree.find_expr state.file callee_id with
                  | Some { desc=BodyArena.EVar name; _ } when IdentPath.equal
                    name
                    (IdentPath.of_name "::") -> true
                  | _ -> false
                )
              | _ ->
                  false
            )
      | BodyArena.ERecord { base_id=None; fields } ->
          fields
          |> List.for_all
            (fun (field: BodyArena.record_expr_field) -> is_nonexpansive_expr state field.value_id)
      | BodyArena.ERecord { base_id=Some _; _ } ->
          false
      | BodyArena.EFieldAccess { receiver_id; _ } ->
          is_nonexpansive_expr state receiver_id
      | BodyArena.EFieldAssign _ ->
          false
      | BodyArena.EIndex _
      | BodyArena.EArray _
      | BodyArena.ESequence _
      | BodyArena.EWhile _
      | BodyArena.EFor _
      | BodyArena.EUnsupported _
      | BodyArena.EHole _ ->
          false
      | BodyArena.ELet (binding_ids, body_id) ->
          List.for_all (is_nonexpansive_binding state) binding_ids && is_nonexpansive_expr state body_id
      | BodyArena.ELocalModule { local_scope; body_id; _ } ->
          List.for_all
            (fun (group: BodyArena.local_module_binding_group) ->
              List.for_all (is_nonexpansive_binding state) group.binding_ids)
            local_scope.binding_groups && is_nonexpansive_expr state body_id
      | BodyArena.EIf (condition_id, then_id, else_id) ->
          is_nonexpansive_expr state condition_id
          && is_nonexpansive_expr state then_id
          && is_nonexpansive_expr state else_id
      | BodyArena.EMatch (scrutinee_id, cases) ->
          is_nonexpansive_expr state scrutinee_id && List.for_all
            (fun (case: BodyArena.match_case) ->
              let guard_nonexpansive =
                match case.guard_id with
                | Some guard_id -> is_nonexpansive_expr state guard_id
                | None -> true
              in
              guard_nonexpansive && is_nonexpansive_expr state case.body_id)
            cases
      | BodyArena.ETry _ ->
          false
      | BodyArena.EPolyVariant { payload; _ } -> (
          match payload with
          | Some payload_id -> is_nonexpansive_expr state payload_id
          | None -> true
        )
      | BodyArena.ECoerce { value_id; _ } ->
          is_nonexpansive_expr state value_id
      | BodyArena.ELocalOpen { body_id; _ } ->
          is_nonexpansive_expr state body_id

and is_nonexpansive_binding = fun (state: state) binding_id ->
  match SemanticTree.find_binding state.file binding_id with
  | Some (binding: BodyArena.binding) -> is_nonexpansive_expr state binding.value_id
  | None -> false

let variances_for_named_type = fun (state: state) head arguments ->
  match visible_type_decl_by_id state head.TypeRepr.type_constructor_id with
  | Some type_decl -> type_decl.declaration.param_variances
  | None -> List.map (fun _ -> TypeDecl.Invariant) arguments

let solver_group_for_entries = fun (state: state) expr_id root_ty entries ->
  let roots = entries |> List.map (fun entry -> TypeScheme.body (Binding.scheme entry)) in
  if is_nonexpansive_expr state expr_id then
    Solver.group roots
  else
    Solver.group ~expansive_roots:[ root_ty ] roots

let origin_of_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_pattern = fun (state: state) pat_id ->
  match SemanticTree.find_pattern state.file pat_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_binding = fun (state: state) (binding: BodyArena.binding) ->
  SemanticTree.find_origin state.file binding.origin_id

let diagnostic_span = fun origin ->
  match origin with
  | Some (origin: OriginMap.origin) -> origin.span
  | None -> empty_span

let zero_arity_named_type = fun (state: state) name ->
  let path = IdentPath.of_name name in
  match BuiltinTypeConstructors.head_of_path path with
  | Some head -> make_type state (TypeRepr.Named { head; arguments = [] })
  | None -> TypeRepr.named_path ~name:path ~arguments:[]

let integer_literal_type = fun (state: state) text ->
  match String.length text with
  | 0 -> TypeRepr.int
  | length -> (
      match String.get text (length - 1) with
      | 'l' -> zero_arity_named_type state "int32"
      | 'L' -> zero_arity_named_type state "int64"
      | 'n' -> zero_arity_named_type state "nativeint"
      | _ -> TypeRepr.int
    )

exception Unify_error of Typ_diagnostic.mismatch

let exact_poly_variant_tags = fun ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.PolyVariant { bound=TypeRepr.Exact; tags; inherited=[] } -> Some tags
  | _ -> None

let merge_exact_poly_variant_tags = fun (state: state) left_tags right_tags ->
  let merged = Collections.HashMap.with_capacity (List.length left_tags + List.length right_tags) in
  let insert_tag (tag: TypeRepr.poly_variant_tag) =
    match Collections.HashMap.get merged tag.name with
    | None ->
        let _ = Collections.HashMap.insert merged tag.name tag in
        Ok ()
    | Some existing_tag -> (
        match (existing_tag.payload_type, tag.payload_type) with
        | (None, None) ->
            Ok ()
        | (Some existing_payload, Some payload) -> (
            match Solver.unify state.solver ~left:existing_payload ~right:payload with
            | Ok () -> Ok ()
            | Error _ -> Error ()
          )
        | _ ->
            Error ()
      )
  in
  let rec add_tags = function
    | [] -> Ok ()
    | tag :: rest -> (
        match insert_tag tag with
        | Ok () -> add_tags rest
        | Error () -> Error ()
      )
  in
  match add_tags left_tags with
  | Error () -> None
  | Ok () -> (
      match add_tags right_tags with
      | Error () -> None
      | Ok () ->
          Some (
            Collections.HashMap.to_list merged |> List.map snd |> List.sort
              (fun (left: TypeRepr.poly_variant_tag) (right: TypeRepr.poly_variant_tag) ->
                String.compare left.name right.name)
          )
    )

let merge_poly_variant_accumulator = fun (state: state) left right ->
  let merge_into_var var linked other =
    match (exact_poly_variant_tags linked, exact_poly_variant_tags other) with
    | (Some linked_tags, Some other_tags) -> (
        match merge_exact_poly_variant_tags state linked_tags other_tags with
        | Some merged_tags ->
            let merged_ty = make_type
              state
              (TypeRepr.PolyVariant { bound = TypeRepr.Exact; tags = merged_tags; inherited = [] }) in
            let () =
              var.TypeRepr.link <- Some merged_ty
            in
            true
        | None -> false
      )
    | _ -> false
  in
  match (TypeRepr.view left, TypeRepr.view right) with
  | (TypeRepr.Var ({ link=Some linked; _ } as var), _) -> merge_into_var var linked right
  | (_, TypeRepr.Var ({ link=Some linked; _ } as var)) -> merge_into_var var linked left
  | _ -> false

let unify = fun (state: state) ~origin:_ left right ->
  let left = normalize_rigid_type state left in
  let right = normalize_rigid_type state right in
  if merge_poly_variant_accumulator state left right then
    ()
  else
    match Solver.unify state.solver ~left ~right with
    | Ok () -> ()
    | Error mismatch -> (
        let canonical_left = canonicalize_type state left in
        let canonical_right = canonicalize_type state right in
        if Std.Ptr.equal left canonical_left && Std.Ptr.equal right canonical_right then
          raise (Unify_error mismatch)
        else
          match Solver.unify state.solver ~left:canonical_left ~right:canonical_right with
          | Ok () -> ()
          | Error mismatch -> raise (Unify_error mismatch)
      )

let try_unify = fun (state: state) ~origin left right ->
  try
    unify state ~origin left right;
    ()
  with
  | Unify_error mismatch -> add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch { mismatch_span = diagnostic_span origin; mismatch })

let add_local_rigid_equation = fun (state: state) rigid_id replacement ->
  let replacement = normalize_rigid_type state replacement in
  if TypeRepr.occurs rigid_id replacement then
    Error (Diagnostic.OccursCheckFailed { variable_id = rigid_id; in_type = TypePrinter.type_to_string replacement })
  else
    (
      State.add_rigid_equation state rigid_id replacement;
      Ok ()
    )

let unify_gadt = fun (state: state) ~origin left right ->
  let rec loop = function
    | [] -> Ok ()
    | (left, right) :: rest ->
        let left = normalize_rigid_type state left |> TypeRepr.prune in
        let right = normalize_rigid_type state right |> TypeRepr.prune in
        if Std.Ptr.equal left right then
          loop rest
        else
          match (TypeRepr.view left, TypeRepr.view right) with
          | (TypeRepr.Int, TypeRepr.Int)
          | (TypeRepr.Float, TypeRepr.Float)
          | (TypeRepr.Bool, TypeRepr.Bool)
          | (TypeRepr.String, TypeRepr.String)
          | (TypeRepr.Char, TypeRepr.Char)
          | (TypeRepr.Unit, TypeRepr.Unit) ->
              loop rest
          | (TypeRepr.Option left_element, TypeRepr.Option right_element)
          | (TypeRepr.Array left_element, TypeRepr.Array right_element)
          | (TypeRepr.List left_element, TypeRepr.List right_element)
          | (TypeRepr.Seq left_element, TypeRepr.Seq right_element) ->
              loop ((left_element, right_element) :: rest)
          | (TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error)) ->
              loop ((left_ok, right_ok) :: (left_error, right_error) :: rest)
          | (TypeRepr.Named { head = left_head; arguments = left_arguments }, TypeRepr.Named {
            head = right_head;
            arguments = right_arguments
          }) ->
              if
                not (TypeConstructorId.equal left_head.type_constructor_id right_head.type_constructor_id)
                || List.length left_arguments != List.length right_arguments
              then
                Error (Diagnostic.ExpectedActual {
                  expected = TypePrinter.type_to_string right;
                  actual = TypePrinter.type_to_string left
                })
              else
                loop (List.rev_append (List.combine left_arguments right_arguments) rest)
          | (TypeRepr.Tuple left_members, TypeRepr.Tuple right_members) ->
              if List.length left_members != List.length right_members then
                Error (Diagnostic.TupleArityMismatch {
                  left = TypePrinter.type_to_string left;
                  right = TypePrinter.type_to_string right;
                  left_arity = List.length left_members;
                  right_arity = List.length right_members
                })
              else
                loop (List.rev_append (List.combine left_members right_members) rest)
          | (TypeRepr.Arrow { label = left_label; lhs = left_lhs; rhs = left_rhs }, TypeRepr.Arrow {
            label = right_label;
            lhs = right_lhs;
            rhs = right_rhs
          }) ->
              if not (labels_match left_label right_label) then
                Error (Diagnostic.ExpectedActual {
                  expected = TypePrinter.type_to_string right;
                  actual = TypePrinter.type_to_string left
                })
              else
                loop ((left_lhs, right_lhs) :: (left_rhs, right_rhs) :: rest)
          | (TypeRepr.Var { id = left_id; kind = TypeRepr.Rigid; _ }, _) -> (
              match add_local_rigid_equation state left_id right with
              | Ok () -> loop rest
              | Error mismatch -> Error mismatch
            )
          | (_, TypeRepr.Var { id = right_id; kind = TypeRepr.Rigid; _ }) -> (
              match add_local_rigid_equation state right_id left with
              | Ok () -> loop rest
              | Error mismatch -> Error mismatch
            )
          | _ ->
              match Solver.unify state.solver ~left ~right with
              | Ok () -> loop rest
              | Error mismatch -> Error mismatch
  in
  match loop [ (left, right) ] with
  | Ok () -> ()
  | Error mismatch ->
      add_diagnostic
        state
        (Typ_diagnostic.TypeMismatch { mismatch_span = diagnostic_span origin; mismatch });
      ()

let package_signature_of_type = fun ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Package signature -> Some signature
  | _ -> None

let instantiate_package_signature = fun (_state: state) (signature: TypeRepr.package_signature) ->
  signature

let check_module_pack_against_signature = fun (state: state) env ~origin module_path ((
  signature: TypeRepr.package_signature
)) ->
  let signature = instantiate_package_signature state signature in
  match Env.lookup_module_scope env module_path with
  | Some scope ->
      let scope_env = env_of_module_scope scope in
      signature.values |> List.iter
        (fun (value: TypeRepr.package_value) ->
          match Env.Value_env.lookup (Env.scope_values scope) (IdentPath.of_name value.name) with
          | Some binding ->
              let actual_ty = instantiate
                state
                (canonicalize_scheme_in_env state scope_env (Binding.scheme binding)) in
              let expected_ty = TypeScheme.body value.scheme in
              try_unify state ~origin actual_ty expected_ty
          | None -> add_diagnostic
            state
            (Typ_diagnostic.UnboundName {
              reference_span = diagnostic_span origin;
              name = IdentPath.append_name module_path value.name |> IdentPath.to_string
            }))
  | None -> add_diagnostic
    state
    (Typ_diagnostic.UnboundName {
      reference_span = diagnostic_span origin;
      name = IdentPath.to_string module_path
    })

let local_module_pack_binding_names = fun (state: state) (local_scope: BodyArena.local_module_scope) ->
  local_scope.binding_groups |> List.concat_map
    (fun (group: BodyArena.local_module_binding_group) ->
      group.binding_ids |> List.filter_map
        (fun binding_id ->
          match SemanticTree.find_binding state.file binding_id with
          | Some ({ name=Some name; _ }: BodyArena.binding) -> Some name
          | _ -> None))

let is_recursive_binding_supported = fun (state: state) (binding: BodyArena.binding) ->
  match binding.name with
  | None -> false
  | Some _ -> (
      match SemanticTree.find_expr state.file binding.value_id with
      | Some value -> (
          match value.desc with
          | BodyArena.EFun _ -> true
          | _ -> false
        )
      | None -> false
    )

let rec constructor_pattern_argument_types = fun (state: state) constructor_ty arguments origin ->
  match arguments with
  | [] -> ([], constructor_ty)
  | _ :: rest ->
      let argument_ty = fresh_var state in
      let result_ty = fresh_var state in
      let () = try_unify
        state
        ~origin
        constructor_ty
        (make_type
          state
          (TypeRepr.Arrow { label = TypeRepr.Nolabel; lhs = argument_ty; rhs = result_ty })) in
      let (rest_argument_types, final_result_ty) = constructor_pattern_argument_types
        state
        result_ty
        rest
        origin in
      (argument_ty :: rest_argument_types, final_result_ty)

and bind_inline_record_pattern = fun (state: state) env pat_id labels fields open_ ->
  let origin = origin_of_pattern state pat_id in
  let field_types = field_types_of_labels state labels in
  let field_names =
    List.map (fun (field: BodyArena.record_pattern_field) -> field.label) fields
  in
  let missing_fields =
    if open_ then
      []
    else
      missing_inline_record_fields labels field_names
  in
  let () =
    if not (List.is_empty missing_fields) then
      add_record_resolution_error
        state
        ~span:(diagnostic_span origin)
        ~context:Typ_diagnostic.RecordPattern (Typ_diagnostic.MissingRecordFields missing_fields)
  in
  fields |> List.fold_left
    (fun acc (field: BodyArena.record_pattern_field) ->
      let field_ty =
        match record_field_type field_types field.label with
        | Some field_ty -> instantiate state field_ty
        | None -> fresh_hole state
      in
      merge_pattern_bindings acc (bind_pattern state env field.pattern_id field_ty))
    empty_pattern_bindings

and bind_argument_patterns = fun (state: state) env arguments argument_types ->
  let rec loop acc arguments argument_types =
    match (arguments, argument_types) with
    | ([], []) -> List.fold_left merge_pattern_bindings empty_pattern_bindings (List.rev acc)
    | (argument_id :: rest_arguments, argument_ty :: rest_types) -> loop
      (bind_pattern state env argument_id argument_ty :: acc)
      rest_arguments
      rest_types
    | (argument_id :: rest_arguments, []) -> loop
      (bind_pattern state env argument_id (fresh_hole state) :: acc)
      rest_arguments
      []
    | ([], _ :: _) -> List.fold_left merge_pattern_bindings empty_pattern_bindings (List.rev acc)
  in
  loop [] arguments argument_types

and bind_pattern = fun (state: state) env pat_id expected_ty ->
  let normalize_bindings bindings = bindings |> Env.of_bindings |> Env.unique |> Env.render in
  let binding_names bindings = bindings |> List.map fst in
  let module_names bindings = bindings.module_entries |> List.map fst |> List.sort String.compare in
  let unify_or_pattern_bindings origin bindings alternatives =
    let expected_bindings = normalize_bindings bindings.entries in
    let expected_names = binding_names expected_bindings in
    let expected_module_names = module_names bindings in
    let rec loop current_bindings remaining =
      match remaining with
      | [] -> Some current_bindings
      | alternative_bindings :: rest ->
          let alternative_entries = normalize_bindings alternative_bindings.entries in
          let actual_names = binding_names alternative_entries in
          let actual_module_names = module_names alternative_bindings in
          if
            (not (List.equal String.equal expected_names actual_names))
            || (not (List.equal String.equal expected_module_names actual_module_names))
          then
            let () = add_or_pattern_bindings_mismatch
              state
              ~span:(diagnostic_span origin)
              ~expected_names
              ~actual_names in
            None
          else
            let () =
              List.iter2
                (fun (_, expected_scheme) (_, actual_scheme) ->
                  try_unify state ~origin (scheme_body expected_scheme) (scheme_body actual_scheme))
                current_bindings
                alternative_entries
            in
            loop current_bindings rest
    in
    match loop expected_bindings alternatives with
    | Some _ -> Some bindings
    | None -> None
  in
  match SemanticTree.find_pattern state.file pat_id with
  | None -> empty_pattern_bindings
  | Some pattern -> (
      let () =
        match pattern.annotation with
        | Some annotation -> try_unify
          state
          ~origin:(origin_of_pattern state pat_id)
          expected_ty
          (canonicalize_type_in_env state env annotation)
        | None -> ()
      in
      match pattern.desc with
      | BodyArena.PVar name ->
          pattern_bindings_of_entries [ generalized_pattern_binding state pat_id ~name expected_ty ]
      | BodyArena.PWildcard ->
          empty_pattern_bindings
      | BodyArena.PInt text ->
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (integer_literal_type state text) in
          empty_pattern_bindings
      | BodyArena.PFloat _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.float in
          empty_pattern_bindings
      | BodyArena.PBool _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.bool in
          empty_pattern_bindings
      | BodyArena.PString _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.string in
          empty_pattern_bindings
      | BodyArena.PChar _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.char in
          empty_pattern_bindings
      | BodyArena.PUnit ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.unit_ in
          empty_pattern_bindings
      | BodyArena.PTuple elements ->
          let element_types =
            List.map (fun _ -> fresh_var state) elements
          in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (make_type state (TypeRepr.Tuple element_types)) in
          List.fold_left2
            (fun acc element_id element_ty ->
              merge_pattern_bindings acc (bind_pattern state env element_id element_ty))
            empty_pattern_bindings
            elements
            element_types
      | BodyArena.POr alternatives -> (
          match alternatives with
          | [] -> empty_pattern_bindings
          | alternative :: rest ->
              let origin = origin_of_pattern state pat_id in
              let bindings = bind_pattern state env alternative expected_ty in
              let alternative_bindings = rest
              |> List.map (fun alternative_id -> bind_pattern state env alternative_id expected_ty) in
              match unify_or_pattern_bindings origin bindings alternative_bindings with
              | Some bindings -> bindings
              | None -> empty_pattern_bindings
        )
      | BodyArena.PConstructor { constructor; arguments } -> (
          match resolve_constructor_entry state env constructor ~expected_ty with
          | Some constructor_entry ->
              let origin = origin_of_pattern state pat_id in
              let instantiated = instantiate_constructor_entry ~pattern_mode:true state env constructor_entry in
              let () =
                if instantiated.generalized then
                  unify_gadt state ~origin expected_ty instantiated.result_ty
                else
                  try_unify state ~origin expected_ty instantiated.result_ty
              in
              (
                match (instantiated.inline_record_labels, arguments) with
                | (Some labels, [ argument_id ]) -> (
                    match SemanticTree.find_pattern state.file argument_id with
                    | Some { desc=BodyArena.PRecord { fields; open_ }; _ } -> bind_inline_record_pattern
                      state
                      env
                      argument_id
                      labels
                      fields
                      open_
                    | _ -> bind_argument_patterns state env arguments instantiated.argument_types
                  )
                | _ -> bind_argument_patterns state env arguments instantiated.argument_types
              )
          | None ->
              let argument_types =
                List.map (fun _ -> fresh_var state) arguments
              in
              bind_argument_patterns state env arguments argument_types
        )
      | BodyArena.PRecord { fields; open_ } -> (
          let origin = origin_of_pattern state pat_id in
          let field_names =
            List.map (fun (field: BodyArena.record_pattern_field) -> field.label) fields
          in
          match resolve_record_decl
            env
            state
            ~field_names
            ~owner_hint:(owner_of_type state expected_ty)
            ~span:(diagnostic_span origin)
            ~context:Typ_diagnostic.RecordPattern with
          | Some record_decl ->
              let (owner_ty, field_types) = instantiate_record_decl state record_decl in
              let () = try_unify state ~origin expected_ty owner_ty in
              let missing_fields =
                if open_ then
                  []
                else
                  Env.Label_env.field_names record_decl |> List.filter
                    (fun label_name ->
                      not
                        (
                          List.exists
                            (fun requested_name ->
                              String.equal (Env.Label_env.lookup_name requested_name) label_name)
                            field_names
                        ))
              in
              let () =
                if not (List.is_empty missing_fields) then
                  add_record_resolution_error
                    state
                    ~span:(diagnostic_span origin)
                    ~context:Typ_diagnostic.RecordPattern (Typ_diagnostic.MissingRecordFields missing_fields)
              in
              fields |> List.fold_left
                (fun acc (field: BodyArena.record_pattern_field) ->
                  let field_ty =
                    match record_field_type field_types field.label with
                    | Some field_ty -> instantiate state field_ty
                    | None -> fresh_hole state
                  in
                  merge_pattern_bindings acc (bind_pattern state env field.pattern_id field_ty))
                empty_pattern_bindings
          | None -> fields
          |> List.fold_left
            (fun acc (field: BodyArena.record_pattern_field) ->
              merge_pattern_bindings acc (bind_pattern state env field.pattern_id (fresh_hole state)))
            empty_pattern_bindings
        )
      | BodyArena.PList elements ->
          let element_ty = fresh_var state in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (make_type state (TypeRepr.List element_ty)) in
          elements
          |> List.fold_left
            (fun acc element_id ->
              merge_pattern_bindings acc (bind_pattern state env element_id element_ty))
            empty_pattern_bindings
      | BodyArena.PAlias { pattern_id; alias } ->
          let bindings = bind_pattern state env pattern_id expected_ty in
          merge_pattern_bindings
            (pattern_bindings_of_entries
              [ generalized_pattern_binding state pat_id ~name:alias expected_ty ])
            bindings
      | BodyArena.PFirstClassModule { module_name; package_type } ->
          let annotated_signature =
            match package_type with
            | Some package_type ->
                let package_type = canonicalize_type_in_env state env package_type in
                package_signature_of_type package_type
            | None -> None
          in
          let expected_signature = package_signature_of_type expected_ty in
          let signature =
            match (annotated_signature, expected_signature) with
            | (Some annotated_signature, Some _expected_signature) ->
                let () = try_unify
                  state
                  ~origin:(origin_of_pattern state pat_id)
                  expected_ty
                  (TypeRepr.package ~values:annotated_signature.values) in
                Some annotated_signature
            | (Some annotated_signature, None) ->
                let () = try_unify
                  state
                  ~origin:(origin_of_pattern state pat_id)
                  expected_ty
                  (TypeRepr.package ~values:annotated_signature.values) in
                Some annotated_signature
            | (None, Some expected_signature) ->
                Some expected_signature
            | (None, None) ->
                let () = add_diagnostic
                  state
                  (Typ_diagnostic.TypeMismatch {
                    mismatch_span = diagnostic_span (origin_of_pattern state pat_id);
                    mismatch = Typ_diagnostic.ExpectedActual {
                      expected = "(module ...)";
                      actual = TypePrinter.type_to_string expected_ty
                    }
                  }) in
                None
          in
          begin
            match (module_name, signature) with
            | (Some module_name, Some signature) -> {
              empty_pattern_bindings
              with module_entries = [ (module_name, package_env state pat_id signature) ]
            }
            | _ -> empty_pattern_bindings
          end
      | BodyArena.PPolyVariant { tag; payload } ->
          let payload_ty =
            match poly_variant_payload_for_expected_type state expected_ty tag with
            | Some payload_ty ->
                payload_ty
            | None when is_unresolved_type expected_ty -> (
                match best_poly_variant_candidate_for_tags state [ tag ] with
                | Some candidate ->
                    let (candidate_ty, mapping) = instantiate_named_type_decl state candidate.type_decl in
                    let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty candidate_ty in
                    instantiate_poly_variant_payload
                      state
                      mapping
                      (poly_variant_candidate_payload candidate tag)
                | None ->
                    let payload_ty =
                      match payload with
                      | Some _ -> Some (fresh_var state)
                      | None -> None
                    in
                    let () = try_unify
                      state
                      ~origin:(origin_of_pattern state pat_id)
                      expected_ty
                      (anonymous_poly_variant_type state tag payload_ty) in
                    payload_ty
              )
            | None ->
                None
          in
          (
            match (payload, payload_ty) with
            | (Some payload_id, Some payload_ty) -> bind_pattern state env payload_id payload_ty
            | (Some payload_id, None) -> bind_pattern state env payload_id (fresh_hole state)
            | (None, _) -> empty_pattern_bindings
          )
      | BodyArena.PUnsupported _ ->
          empty_pattern_bindings
    )

let record_expr_trace = fun (state: state) expr_id origin_id env_before inferred_type ->
  let binding_provenance = function
    | Binding.LoweredPattern pat_id -> Check_result.LoweredPattern pat_id
    | Binding.Prelude -> Check_result.Prelude
    | Binding.Ambient -> Check_result.Ambient
    | Binding.TypeConstructor { type_name; scope_path } -> Check_result.TypeConstructor {
      type_name;
      scope_path
    }
    | Binding.Exception { name; scope_path } -> Check_result.Exception { name; scope_path }
    | Binding.DeclaredValue { name; scope_path } -> Check_result.DeclaredValue { name; scope_path }
    | Binding.Included { module_path } -> Check_result.Included { module_path }
    | Binding.ModuleAlias { alias_name; module_path } -> Check_result.ModuleAlias {
      alias_name;
      module_path
    }
  in
  let resolved_binding =
    match SemanticTree.find_expr state.file expr_id with
    | Some { desc=BodyArena.EVar name; _ } -> Env.lookup env_before name
    |> Option.map
      (fun binding ->
        {
          Check_result.path = Binding.path binding;
          provenance = binding_provenance (Binding.provenance binding)
        })
    | _ -> None
  in
  if state.config.capture_traces then
    state.expr_traces <- ({
        Check_result.expr_id;
        origin_id;
        env_before = Env.render env_before;
        resolved_binding;
        inferred_type;
      }: Check_result.expr_trace) :: state.expr_traces

let binding_ref_of_binding = fun binding ->
  let provenance =
    match Binding.provenance binding with
    | Binding.LoweredPattern pat_id -> Check_result.LoweredPattern pat_id
    | Binding.Prelude -> Check_result.Prelude
    | Binding.Ambient -> Check_result.Ambient
    | Binding.TypeConstructor { type_name; scope_path } -> Check_result.TypeConstructor {
      type_name;
      scope_path
    }
    | Binding.Exception { name; scope_path } -> Check_result.Exception { name; scope_path }
    | Binding.DeclaredValue { name; scope_path } -> Check_result.DeclaredValue { name; scope_path }
    | Binding.Included { module_path } -> Check_result.Included { module_path }
    | Binding.ModuleAlias { alias_name; module_path } -> Check_result.ModuleAlias {
      alias_name;
      module_path
    }
  in
  { Check_result.path = Binding.path binding; provenance }

let export_binding_refs = fun env ->
  Env.unique env |> Env.bindings |> List.sort
    (fun left right ->
      IdentPath.compare (Binding.path left) (Binding.path right)) |> List.map binding_ref_of_binding

let add_application_label_mismatch = fun (state: state) ~expr_id ~expected_label arguments ->
  add_diagnostic
    state
    (Typ_diagnostic.ApplicationLabelMismatch {
      application_span = diagnostic_span (origin_of_expr state expr_id);
      expected_label = diagnostic_label_of_type_label expected_label;
      actual_labels = List.map
        (fun (argument: BodyArena.apply_argument) -> diagnostic_label_of_body_label argument.label)
        arguments
    })

let add_application_non_function = fun (state: state) ~expr_id current_ty ->
  add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch {
      mismatch_span = diagnostic_span (origin_of_expr state expr_id);
      mismatch = Typ_diagnostic.ExpectedActual {
        expected = "function";
        actual = TypePrinter.type_to_string current_ty
      }
    })

let add_nonexhaustive_match = fun (state: state) ~expr_id witness ->
  add_diagnostic
    state
    (Typ_diagnostic.NonexhaustiveMatch {
      match_span = diagnostic_span (origin_of_expr state expr_id);
      witness
    })

let add_redundant_match_case = fun (state: state) (case: BodyArena.match_case) ->
  add_diagnostic
    state
    (Typ_diagnostic.RedundantMatchCase {
      case_span = diagnostic_span (origin_of_pattern state case.pattern_id)
    })

let analyze_match_coverage = fun (state: state) ~expr_id scrutinee_ty cases ->
  let rec loop rows cases =
    match cases with
    | [] -> useful_coverage_pattern state rows CoverageAny scrutinee_ty
    |> Option.iter (fun witness -> add_nonexhaustive_match state ~expr_id witness)
    | (case: BodyArena.match_case) :: rest -> (
        match coverage_patterns_of_pattern state case.pattern_id scrutinee_ty with
        | None -> ()
        | Some patterns ->
            let useful_case = patterns
            |> List.exists
              (fun pattern ->
                Option.is_some (useful_coverage_pattern state rows pattern scrutinee_ty)) in
            let () =
              if not useful_case then
                add_redundant_match_case state case
            in
            let next_rows =
              match case.guard_id with
              | Some _ -> rows
              | None -> rows @ (patterns |> List.map (fun pattern -> [ pattern ]))
            in
            loop next_rows rest
      )
  in
  loop [] cases

let rec infer_match_case = fun (state: state) env scrutinee_ty result_ty (case: BodyArena.match_case) ->
  let bindings = bind_pattern state env case.pattern_id scrutinee_ty in
  let case_env = env_with_pattern_bindings env bindings in
  let () =
    match case.guard_id with
    | Some guard_id ->
        let guard_ty = infer_expr state case_env guard_id in
        try_unify state ~origin:(origin_of_expr state guard_id) guard_ty TypeRepr.bool
    | None -> ()
  in
  let case_ty = infer_expr state case_env case.body_id in
  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty

and infer_match_case_against = fun (state: state) env scrutinee_ty expected_ty (case: BodyArena.match_case) ->
  State.with_local_rigid_equations state
    (fun () ->
      let bindings = bind_pattern state env case.pattern_id scrutinee_ty in
      let case_env = env_with_pattern_bindings env bindings in
      let () =
        match case.guard_id with
        | Some guard_id ->
            let guard_ty = infer_expr state case_env guard_id in
            try_unify state ~origin:(origin_of_expr state guard_id) guard_ty TypeRepr.bool
        | None -> ()
      in
      let _ = infer_expr_against state case_env case.body_id expected_ty in
      ())

and infer_inline_record_expr = fun (state: state) env expr_id fields labels ->
  let field_types = field_types_of_labels state labels in
  let field_names =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  let missing_fields = missing_inline_record_fields labels field_names in
  let () =
    if not (List.is_empty missing_fields) then
      add_record_resolution_error
        state
        ~span:(diagnostic_span (origin_of_expr state expr_id))
        ~context:Typ_diagnostic.RecordConstruction (Typ_diagnostic.MissingRecordFields missing_fields)
  in
  List.iter
    (fun (field: BodyArena.record_expr_field) ->
      let field_ty =
        match record_field_type field_types field.label with
        | Some field_ty -> instantiate state field_ty
        | None -> fresh_hole state
      in
      let inferred_field_ty = infer_expr state env field.value_id in
      try_unify state ~origin:(origin_of_expr state field.value_id) field_ty inferred_field_ty)
    fields

and infer_record_expr = fun (state: state) env expr_id base_id fields ->
  let operation_span = diagnostic_span (origin_of_expr state expr_id) in
  let base_ty =
    match base_id with
    | Some base_id -> Some (infer_expr state env base_id)
    | None -> None
  in
  let field_names =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  let context =
    match base_id with
    | Some _ -> Typ_diagnostic.RecordUpdate
    | None -> Typ_diagnostic.RecordConstruction
  in
  match
    resolve_record_decl env state ~field_names
      ~owner_hint:(
        match base_ty with
        | Some base_ty -> owner_of_type state base_ty
        | None -> None
      )
      ~span:operation_span
      ~context
  with
  | Some record_decl ->
      let (owner_ty, field_types) = instantiate_record_decl state record_decl in
      let () =
        match base_id, base_ty with
        | Some base_id, Some base_ty -> try_unify
          state
          ~origin:(origin_of_expr state base_id)
          base_ty
          owner_ty
        | _ -> ()
      in
      let missing_fields =
        match base_id with
        | Some _ -> []
        | None ->
            Env.Label_env.field_names record_decl |> List.filter
              (fun label_name ->
                not
                  (
                    List.exists
                      (fun requested_name ->
                        String.equal (Env.Label_env.lookup_name requested_name) label_name)
                      field_names
                  ))
      in
      let () =
        if not (List.is_empty missing_fields) then
          add_record_resolution_error
            state
            ~span:operation_span
            ~context
            (Typ_diagnostic.MissingRecordFields missing_fields)
      in
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let field_ty =
              match record_field_type field_types field.label with
              | Some field_ty -> instantiate state field_ty
              | None -> fresh_hole state
            in
            let inferred_field_ty = infer_expr state env field.value_id in
            try_unify state ~origin:(origin_of_expr state field.value_id) field_ty inferred_field_ty)
          fields
      in
      owner_ty
  | None ->
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let _ = infer_expr state env field.value_id in
            ())
          fields
      in
      fresh_hole state

and infer_expr = fun (state: state) env expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> fresh_hole state
  | Some expr ->
      let inferred_type =
        match expr.desc with
        | BodyArena.EVar name -> (
            match Env.lookup env name with
            | Some binding ->
                let scheme = canonicalize_scheme_in_env state env (Binding.scheme binding) in
                instantiate state scheme
            | None -> (
                match origin_of_expr state expr_id with
                | Some origin when String.equal origin.label "constructor_expression"
                || String.equal origin.label "constructor_path_expression" -> (
                    match resolve_constructor_without_expected env name with
                    | Some constructor_entry ->
                        let scheme = canonicalize_scheme_in_env
                          state
                          env
                          (Env.Constructor_env.scheme constructor_entry) in
                        instantiate state scheme
                    | None ->
                        let hole = fresh_hole state in
                        let () = add_diagnostic
                          state
                          (Typ_diagnostic.UnboundName {
                            reference_span = diagnostic_span (origin_of_expr state expr_id);
                            name = IdentPath.to_string name
                          }) in
                        hole
                  )
                | _ ->
                    let hole = fresh_hole state in
                    let () = add_diagnostic
                      state
                      (Typ_diagnostic.UnboundName {
                        reference_span = diagnostic_span (origin_of_expr state expr_id);
                        name = IdentPath.to_string name
                      }) in
                    hole
              )
          )
        | BodyArena.EInt text ->
            integer_literal_type state text
        | BodyArena.EFloat _ ->
            TypeRepr.float
        | BodyArena.EBool _ ->
            TypeRepr.bool
        | BodyArena.EString _ ->
            TypeRepr.string
        | BodyArena.EChar _ ->
            TypeRepr.char
        | BodyArena.EUnit ->
            TypeRepr.unit_
        | BodyArena.ETuple elements ->
            make_type state (TypeRepr.Tuple (List.map (infer_expr state env) elements))
        | BodyArena.EArray elements ->
            let element_ty = fresh_var state in
            let () =
              List.iter
                (fun element_id ->
                  let inferred_element = infer_expr state env element_id in
                  try_unify state ~origin:(origin_of_expr state element_id) element_ty inferred_element)
                elements
            in
            make_type state (TypeRepr.Array element_ty)
        | BodyArena.ESequence elements -> (
            match List.rev elements with
            | [] -> TypeRepr.unit_
            | last_id :: reversed_prefix ->
                let prefix = List.rev reversed_prefix in
                let () =
                  List.iter
                    (fun element_id ->
                      let element_ty = infer_expr state env element_id in
                      try_unify state ~origin:(origin_of_expr state element_id) element_ty TypeRepr.unit_)
                    prefix
                in
                infer_expr state env last_id
          )
        | BodyArena.EWhile { condition_id; body_id } ->
            let condition_ty = infer_expr state env condition_id in
            let () = try_unify
              state
              ~origin:(origin_of_expr state condition_id)
              condition_ty
              TypeRepr.bool in
            let body_ty = infer_expr state env body_id in
            let () = try_unify
              state
              ~origin:(origin_of_expr state body_id)
              body_ty
              TypeRepr.unit_ in
            TypeRepr.unit_
        | BodyArena.EFor {
          iterator_pattern_id;
          start_id;
          end_id;
          body_id;
          _
        } ->
            let start_ty = infer_expr state env start_id in
            let end_ty = infer_expr state env end_id in
            let () = try_unify state ~origin:(origin_of_expr state start_id) start_ty TypeRepr.int in
            let () = try_unify state ~origin:(origin_of_expr state end_id) end_ty TypeRepr.int in
            let iterator_bindings = bind_pattern state env iterator_pattern_id TypeRepr.int in
            let body_env = env_with_pattern_bindings env iterator_bindings in
            let body_ty = infer_expr state body_env body_id in
            let () = try_unify state ~origin:(origin_of_expr state body_id) body_ty TypeRepr.unit_ in
            TypeRepr.unit_
        | BodyArena.EFun (parameters, body_id) ->
            let rec lower_parameters env = function
              | [] -> infer_expr state env body_id
              | (parameter: BodyArena.function_parameter) :: rest ->
                  let arg_ty = fresh_var state in
                  let bound_ty =
                    match (parameter.label, parameter.default_value_id) with
                    | (BodyArena.Optional _, None) -> TypeRepr.option arg_ty
                    | _ -> arg_ty
                  in
                  let () =
                    match parameter.default_value_id with
                    | Some default_value_id ->
                        let _ = infer_expr_against state env default_value_id arg_ty in
                        ()
                    | None -> ()
                  in
                  let bindings = bind_pattern state env parameter.pattern_id bound_ty in
                  let body_ty = lower_parameters (env_with_pattern_bindings env bindings) rest in
                  make_type
                    state
                    (TypeRepr.Arrow {
                      label = type_label_of_body_label parameter.label;
                      lhs = arg_ty;
                      rhs = body_ty
                    })
            in
            lower_parameters env parameters
        | BodyArena.EApply (callee_id, arguments) ->
            let callee_ty = infer_expr state env callee_id in
            let infer_apply_argument_against (argument: BodyArena.apply_argument) lhs =
              match (argument.label, argument.implicit) with
              | (BodyArena.Optional _, true) ->
                  let forwarded_option_ty = infer_expr state env argument.value_id in
                  let forwarded_value_ty = fresh_var state in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    forwarded_option_ty
                    (TypeRepr.option forwarded_value_ty) in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    forwarded_value_ty
                    lhs in
                  infer_expr_against state env argument.value_id (TypeRepr.option lhs)
              | _ -> infer_expr_against state env argument.value_id lhs
            in
            let rec apply_with_known_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | _ -> (
                  match view current_ty with
                  | TypeRepr.Arrow { label; lhs; rhs } -> (
                      match take_matching_argument label arguments with
                      | Some ((argument: BodyArena.apply_argument), rest_arguments) ->
                          let _ = infer_apply_argument_against argument lhs in
                          apply_with_known_type rhs rest_arguments
                      | None -> (
                          match label with
                          | TypeRepr.Optional _ -> apply_with_known_type rhs arguments
                          | _ ->
                              let () = add_application_label_mismatch
                                state
                                ~expr_id
                                ~expected_label:label
                                arguments in
                              fresh_hole state
                        )
                    )
                  | TypeRepr.Var _
                  | TypeRepr.Hole _ ->
                      apply_from_unknown_type current_ty arguments
                  | _ ->
                      let () = add_application_non_function state ~expr_id current_ty in
                      fresh_hole state
                )
            and apply_from_unknown_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | (argument: BodyArena.apply_argument) :: rest ->
                  let argument_ty =
                    match (argument.label, argument.implicit) with
                    | (BodyArena.Optional _, true) ->
                        let forwarded_option_ty = infer_expr state env argument.value_id in
                        let forwarded_value_ty = fresh_var state in
                        let () = try_unify
                          state
                          ~origin:(origin_of_expr state expr_id)
                          forwarded_option_ty
                          (TypeRepr.option forwarded_value_ty) in
                        forwarded_value_ty
                    | _ -> infer_expr state env argument.value_id
                  in
                  let result_ty = fresh_var state in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    current_ty
                    (make_type
                      state
                      (TypeRepr.Arrow {
                        label = type_label_of_body_label argument.label;
                        lhs = argument_ty;
                        rhs = result_ty
                      })) in
                  apply_with_known_type result_ty rest
            in
            (
              match origin_of_expr state expr_id with
              | Some origin when String.equal origin.label "constructor_apply_expression" -> (
                  match SemanticTree.find_expr state.file callee_id with
                  | Some { desc=BodyArena.EVar constructor; _ } -> (
                      match resolve_constructor_without_expected env constructor with
                      | Some constructor_entry ->
                          let instantiated = instantiate_constructor_entry state env constructor_entry in
                          let constructor_ty =
                            List.fold_right
                              (fun argument_ty result_ty ->
                                make_type
                                  state
                                  (TypeRepr.Arrow {
                                    label = TypeRepr.Nolabel;
                                    lhs = argument_ty;
                                    rhs = result_ty
                                  }))
                              instantiated.argument_types
                              instantiated.result_ty
                          in
                          (
                            match (instantiated.inline_record_labels, arguments) with
                            | (Some labels, [ argument ]) when argument_matches_parameter_label
                              TypeRepr.Nolabel
                              argument.label -> (
                                match SemanticTree.find_expr state.file argument.value_id with
                                | Some { desc=BodyArena.ERecord { base_id=None; fields }; _ } ->
                                    let () = infer_inline_record_expr
                                      state
                                      env
                                      argument.value_id
                                      fields
                                      labels in
                                    instantiated.result_ty
                                | _ -> apply_with_known_type constructor_ty arguments
                              )
                            | _ -> apply_with_known_type constructor_ty arguments
                          )
                      | None -> apply_with_known_type callee_ty arguments
                    )
                  | _ -> apply_with_known_type callee_ty arguments
                )
              | _ -> apply_with_known_type callee_ty arguments
            )
        | BodyArena.ERecord { base_id; fields } ->
            infer_record_expr state env expr_id base_id fields
        | BodyArena.EFieldAccess { receiver_id; label } ->
            let receiver_ty = infer_expr state env receiver_id in
            let field_names = [ label ] in
            begin
              match resolve_record_decl
                env
                state
                ~field_names
                ~owner_hint:(owner_of_type state receiver_ty)
                ~span:(diagnostic_span (origin_of_expr state expr_id))
                ~context:Typ_diagnostic.RecordFieldAccess with
              | Some record_decl ->
                  let (owner_ty, field_types) = instantiate_record_decl state record_decl in
                  let () = try_unify state ~origin:(origin_of_expr state receiver_id) receiver_ty owner_ty in
                  begin
                    match record_field_type field_types label with
                    | Some field_ty -> instantiate state field_ty
                    | None -> fresh_hole state
                  end
              | None -> fresh_hole state
            end
        | BodyArena.EFieldAssign { receiver_id; label; value_id } ->
            let receiver_ty = infer_expr state env receiver_id in
            let field_names = [ label ] in
            begin
              match resolve_record_decl
                env
                state
                ~field_names
                ~owner_hint:(owner_of_type state receiver_ty)
                ~span:(diagnostic_span (origin_of_expr state expr_id))
                ~context:Typ_diagnostic.RecordUpdate with
              | Some record_decl ->
                  let (owner_ty, field_types) = instantiate_record_decl state record_decl in
                  let () = try_unify state ~origin:(origin_of_expr state receiver_id) receiver_ty owner_ty in
                  begin
                    match record_field_type field_types label with
                    | Some field_ty ->
                        let value_ty = infer_expr state env value_id in
                        let field_ty = instantiate state field_ty in
                        let () = try_unify state ~origin:(origin_of_expr state value_id) value_ty field_ty in
                        TypeRepr.unit_
                    | None -> fresh_hole state
                  end
              | None ->
                  let _ = infer_expr state env value_id in
                  fresh_hole state
            end
        | BodyArena.EIndex (collection_id, index_id) ->
            let collection_ty = infer_expr state env collection_id in
            let index_ty = infer_expr state env index_id in
            let element_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state index_id) index_ty TypeRepr.int in
            begin
              match view collection_ty with
              | TypeRepr.String -> TypeRepr.char
              | _ ->
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state collection_id)
                    collection_ty
                    (make_type state (TypeRepr.Array element_ty)) in
                  element_ty
            end
        | BodyArena.ELet (binding_ids, body_id) ->
            let env = infer_binding_group state env binding_ids in
            infer_expr state env body_id
        | BodyArena.EIf (condition_id, then_id, else_id) ->
            let condition_ty = infer_expr state env condition_id in
            let () = try_unify
              state
              ~origin:(origin_of_expr state condition_id)
              condition_ty
              TypeRepr.bool in
            let then_ty = infer_expr state env then_id in
            let else_ty = infer_expr state env else_id in
            let () = try_unify state ~origin:(origin_of_expr state expr_id) then_ty else_ty in
            then_ty
        | BodyArena.EMatch (scrutinee_id, cases) ->
            let scrutinee_ty = infer_expr state env scrutinee_id in
            let () = constrain_match_scrutinee_to_poly_variant state scrutinee_ty cases in
            let result_ty = fresh_var state in
            let () = List.iter (infer_match_case state env scrutinee_ty result_ty) cases in
            let () = analyze_match_coverage state ~expr_id scrutinee_ty cases in
            result_ty
        | BodyArena.ETry (body_id, cases) ->
            let body_ty = infer_expr state env body_id in
            let exn_ty = make_type
              state
              (TypeRepr.Named {
                head = TypeRepr.named_head
                  ~type_constructor_id:BuiltinTypeConstructors.exn_type_constructor_id
                  ~name:(IdentPath.of_name "exn");
                arguments = []
              }) in
            let result_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state body_id) result_ty body_ty in
            let () = List.iter (infer_match_case state env exn_ty result_ty) cases in
            let () = analyze_match_coverage state ~expr_id exn_ty cases in
            result_ty
        | BodyArena.EPolyVariant { tag; payload } -> (
            match best_poly_variant_candidate_for_tags state [ tag ] with
            | Some candidate ->
                let (candidate_ty, mapping) = instantiate_named_type_decl state candidate.type_decl in
                let payload_ty = instantiate_poly_variant_payload
                  state
                  mapping
                  (poly_variant_candidate_payload candidate tag) in
                let () =
                  match (payload, payload_ty) with
                  | (Some payload_id, Some payload_ty) ->
                      let _ = infer_expr_against state env payload_id payload_ty in
                      ()
                  | (Some payload_id, None) ->
                      let _ = infer_expr state env payload_id in
                      ()
                  | (None, _) ->
                      ()
                in
                candidate_ty
            | None ->
                let payload_ty =
                  match payload with
                  | Some payload_id -> Some (infer_expr state env payload_id)
                  | None -> None
                in
                anonymous_poly_variant_type state tag payload_ty
          )
        | BodyArena.ECoerce { value_id; target_type } ->
            let source_ty = infer_expr state env value_id in
            let target_type = canonicalize_type_in_env state env target_type in
            let () =
              if can_explicitly_coerce_poly_variant state ~source_ty ~target_ty:target_type then
                ()
              else
                try_unify state ~origin:(origin_of_expr state expr_id) source_ty target_type
            in
            target_type
        | BodyArena.EModulePack { module_path; package_type } -> (
            match package_type with
            | Some package_type -> (
                let package_type = canonicalize_type_in_env state env package_type in
                match package_signature_of_type package_type with
                | Some signature ->
                    let () = check_module_pack_against_signature
                      state
                      env
                      ~origin:(origin_of_expr state expr_id)
                      module_path
                      signature in
                    package_type
                | None ->
                    let () = add_diagnostic
                      state
                      (Typ_diagnostic.TypeMismatch {
                        mismatch_span = diagnostic_span (origin_of_expr state expr_id);
                        mismatch = Typ_diagnostic.ExpectedActual {
                          expected = "(module ...)";
                          actual = TypePrinter.type_to_string package_type
                        }
                      }) in
                    fresh_hole state
              )
            | None ->
                let () = add_diagnostic
                  state
                  (Typ_diagnostic.TypeMismatch {
                    mismatch_span = diagnostic_span (origin_of_expr state expr_id);
                    mismatch = Typ_diagnostic.ExpectedActual {
                      expected = "(module S)";
                      actual = "(module ...)"
                    }
                  }) in
                fresh_hole state
          )
        | BodyArena.ELocalModulePack { local_scope; package_type } -> (
            match package_type with
            | Some package_type -> (
                let package_type = canonicalize_type_in_env state env package_type in
                match package_signature_of_type package_type with
                | Some signature ->
                    let () = check_local_module_pack_against_signature
                      state
                      env
                      ~origin:(origin_of_expr state expr_id)
                      local_scope
                      signature in
                    package_type
                | None ->
                    let () = add_diagnostic
                      state
                      (Typ_diagnostic.TypeMismatch {
                        mismatch_span = diagnostic_span (origin_of_expr state expr_id);
                        mismatch = Typ_diagnostic.ExpectedActual {
                          expected = "(module ...)";
                          actual = TypePrinter.type_to_string package_type
                        }
                      }) in
                    fresh_hole state
              )
            | None ->
                let () = add_diagnostic
                  state
                  (Typ_diagnostic.TypeMismatch {
                    mismatch_span = diagnostic_span (origin_of_expr state expr_id);
                    mismatch = Typ_diagnostic.ExpectedActual {
                      expected = "(module S)";
                      actual = "(module ...)"
                    }
                  }) in
                fresh_hole state
          )
        | BodyArena.ELocalModule { module_name; local_scope; body_id } ->
            let local_module_env = infer_local_module_env state env local_scope in
            let body_env = Env.bind env (Env.singleton_module ~name:module_name local_module_env) in
            infer_expr state body_env body_id
        | BodyArena.ELocalOpen { module_path; body_id } ->
            infer_expr state (Env.with_local_open env module_path) body_id
        | BodyArena.EUnsupported summary ->
            let hole = fresh_hole state in
            let () = add_diagnostic
              state
              (Typ_diagnostic.UnsupportedSemanticExpression {
                expression_span = diagnostic_span (origin_of_expr state expr_id);
                summary
              }) in
            hole
        | BodyArena.EHole _ ->
            fresh_hole state
      in
      let () = record_expr_trace state expr_id expr.origin_id env inferred_type in
      inferred_type

and infer_expr_against = fun (state: state) env expr_id expected_ty ->
  match SemanticTree.find_expr state.file expr_id with
  | Some expr -> (
      match expr.desc with
      | BodyArena.EFun (parameters, body_id) ->
          State.with_local_rigid_equations state
            (fun () ->
              let rec check_parameters env current_expected = function
                | [] ->
                    let _ = infer_expr_against state env body_id current_expected in
                    expected_ty
                | (parameter: BodyArena.function_parameter) :: rest -> (
                    match TypeRepr.view (normalize_rigid_type state current_expected) with
                    | TypeRepr.Arrow { label; lhs; rhs } when labels_match label
                      (type_label_of_body_label parameter.label) ->
                        let () =
                          match parameter.default_value_id with
                          | Some default_value_id ->
                              let _ = infer_expr_against state env default_value_id lhs in
                              ()
                          | None -> ()
                        in
                        let bound_ty =
                          match (parameter.label, parameter.default_value_id) with
                          | (BodyArena.Optional _, None) -> TypeRepr.option lhs
                          | _ -> lhs
                        in
                        let bindings = bind_pattern state env parameter.pattern_id bound_ty in
                        check_parameters (env_with_pattern_bindings env bindings) rhs rest
                    | _ ->
                        let inferred_type = infer_expr state env expr_id in
                        let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                        inferred_type
                  )
              in
              check_parameters env expected_ty parameters)
      | BodyArena.EMatch (scrutinee_id, cases) ->
          let scrutinee_ty = infer_expr state env scrutinee_id in
          let () = constrain_match_scrutinee_to_poly_variant state scrutinee_ty cases in
          let () = List.iter (infer_match_case_against state env scrutinee_ty expected_ty) cases in
          let () = analyze_match_coverage state ~expr_id scrutinee_ty cases in
          expected_ty
      | BodyArena.EVar name -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_expression"
          || String.equal origin.label "constructor_path_expression" -> (
              match resolve_constructor_entry state env name ~expected_ty with
              | Some constructor_entry ->
                  let scheme = canonicalize_scheme_in_env
                    state
                    env
                    (Env.Constructor_env.scheme constructor_entry) in
                  let inferred_type = instantiate state scheme in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
              | None ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | BodyArena.EApply (callee_id, arguments) -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_apply_expression" -> (
              match SemanticTree.find_expr state.file callee_id with
              | Some { desc=BodyArena.EVar constructor; _ } -> (
                  match resolve_constructor_entry state env constructor ~expected_ty with
                  | Some constructor_entry ->
                      let instantiated = instantiate_constructor_entry state env constructor_entry in
                      let callee_ty =
                        List.fold_right
                          (fun argument_ty result_ty ->
                            make_type
                              state
                              (TypeRepr.Arrow {
                                label = TypeRepr.Nolabel;
                                lhs = argument_ty;
                                rhs = result_ty
                              }))
                          instantiated.argument_types
                          instantiated.result_ty
                      in
                      let rec apply_with_known_type current_ty arguments =
                        match arguments with
                        | [] ->
                            let () = try_unify
                              state
                              ~origin:(origin_of_expr state expr_id)
                              expected_ty
                              current_ty in
                            current_ty
                        | (argument: BodyArena.apply_argument) :: rest -> (
                            match view current_ty with
                            | TypeRepr.Arrow { label; lhs; rhs } ->
                                if argument_matches_parameter_label label argument.label then
                                  let _ = infer_expr_against state env argument.value_id lhs in
                                  apply_with_known_type rhs rest
                                else
                                  let inferred_type = infer_expr state env expr_id in
                                  let () = try_unify
                                    state
                                    ~origin:(origin_of_expr state expr_id)
                                    expected_ty
                                    inferred_type in
                                  inferred_type
                            | _ ->
                                let inferred_type = infer_expr state env expr_id in
                                let () = try_unify
                                  state
                                  ~origin:(origin_of_expr state expr_id)
                                  expected_ty
                                  inferred_type in
                                inferred_type
                          )
                      in
                      (
                        match (instantiated.inline_record_labels, arguments) with
                        | (Some labels, [ argument ]) when argument_matches_parameter_label
                          TypeRepr.Nolabel
                          argument.label -> (
                            match SemanticTree.find_expr state.file argument.value_id with
                            | Some { desc=BodyArena.ERecord { base_id=None; fields }; _ } ->
                                let () = infer_inline_record_expr
                                  state
                                  env
                                  argument.value_id
                                  fields
                                  labels in
                                let () = try_unify
                                  state
                                  ~origin:(origin_of_expr state expr_id)
                                  expected_ty
                                  instantiated.result_ty in
                                instantiated.result_ty
                            | _ -> apply_with_known_type callee_ty arguments
                          )
                        | _ -> apply_with_known_type callee_ty arguments
                      )
                  | None ->
                      let inferred_type = infer_expr state env expr_id in
                      let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                      inferred_type
                )
              | _ ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | BodyArena.EPolyVariant { tag; payload } -> (
          match poly_variant_payload_for_expected_type state expected_ty tag with
          | Some payload_ty ->
              let () =
                match (payload, payload_ty) with
                | (Some payload_id, Some payload_ty) ->
                    let _ = infer_expr_against state env payload_id payload_ty in
                    ()
                | (Some payload_id, None) ->
                    let _ = infer_expr state env payload_id in
                    ()
                | (None, _) -> ()
              in
              expected_ty
          | None ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | BodyArena.EModulePack { module_path; package_type } -> (
          let inferred_type =
            match (package_type, package_signature_of_type expected_ty) with
            | (_, Some signature) ->
                let () = check_module_pack_against_signature
                  state
                  env
                  ~origin:(origin_of_expr state expr_id)
                  module_path
                  signature in
                expected_ty
            | (Some package_type, None) ->
                let package_type = canonicalize_type_in_env state env package_type in
                let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty package_type in
                package_type
            | (None, None) ->
                infer_expr state env expr_id
          in
          let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
          inferred_type
        )
      | BodyArena.ELocalModulePack { local_scope; package_type } -> (
          let inferred_type =
            match (package_type, package_signature_of_type expected_ty) with
            | (_, Some signature) ->
                let () = check_local_module_pack_against_signature
                  state
                  env
                  ~origin:(origin_of_expr state expr_id)
                  local_scope
                  signature in
                expected_ty
            | (Some package_type, None) ->
                let package_type = canonicalize_type_in_env state env package_type in
                let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty package_type in
                package_type
            | (None, None) ->
                infer_expr state env expr_id
          in
          let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
          inferred_type
        )
      | _ ->
          let inferred_type = infer_expr state env expr_id in
          let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
          inferred_type
    )
  | None -> fresh_hole state

and infer_local_module_env = fun (state: state) env (local_scope: BodyArena.local_module_scope) ->
  let local_type_env = Env.of_type_decls local_scope.type_decls in
  let env_with_local_types = Env.bind env local_type_env in
  let env_after_bindings = local_scope.binding_groups
  |> List.fold_left
    (fun current_env (group: BodyArena.local_module_binding_group) ->
      infer_binding_group state current_env group.binding_ids)
    env_with_local_types in
  let introduced_entries = Env.introduced_entries env_with_local_types env_after_bindings in
  Env.bind local_type_env introduced_entries

and check_local_module_pack_against_signature = fun (state: state) env ~origin (local_scope: BodyArena.local_module_scope) ((
  signature: TypeRepr.package_signature
)) ->
  let signature = instantiate_package_signature state signature in
  let local_binding_names = local_module_pack_binding_names state local_scope in
  let local_type_env = Env.of_type_decls local_scope.type_decls in
  let local_env = local_scope.binding_groups
  |> List.fold_left
    (fun current_env (group: BodyArena.local_module_binding_group) ->
      infer_binding_group state current_env group.binding_ids)
    (Env.bind env local_type_env) in
  signature.values |> List.iter
    (fun (value: TypeRepr.package_value) ->
      if not (List.mem value.name local_binding_names) then
        add_diagnostic
          state
          (Typ_diagnostic.UnboundName { reference_span = diagnostic_span origin; name = value.name })
      else
        match Env.lookup local_env (IdentPath.of_name value.name) with
        | Some binding ->
            let actual_ty = instantiate
              state
              (canonicalize_scheme_in_env state local_env (Binding.scheme binding)) in
            let expected_ty = TypeScheme.body value.scheme in
            try_unify state ~origin actual_ty expected_ty
        | None -> add_diagnostic
          state
          (Typ_diagnostic.UnboundName { reference_span = diagnostic_span origin; name = value.name }))

and infer_binding_group = fun (state: state) env binding_ids ->
  let bindings = binding_ids |> List.filter_map (SemanticTree.find_binding state.file) in
  let recursive =
    match bindings with
    | [] -> false
    | (binding: BodyArena.binding) :: _ -> binding.recursive
  in
  if recursive then
    infer_recursive_group state env bindings
  else
    infer_nonrecursive_group state env bindings

and exported_schemes_for_binding = fun annotation_scheme (binding: BodyArena.binding) entries schemes ->
  match (annotation_scheme, binding.name, entries, schemes) with
  | (Some annotation, Some _, [ _ ], [ _ ]) -> [ annotation ]
  | _ -> schemes

and infer_nonrecursive_group = fun (state: state) env bindings ->
  let (inferred_bindings, generalized_groups) =
    Solver.with_local_level_gen state.solver ~variance_of_named:(variances_for_named_type state)
      (fun () ->
        let inferred_bindings =
          List.map
            (fun (binding: BodyArena.binding) ->
              let export_annotation_scheme = binding.annotation
              |> Option.map (canonicalize_generalized_scheme_in_env state env) in
              let checking_annotation_scheme = binding.annotation
              |> Option.map (canonicalize_scheme_in_env state env) in
              let value_ty =
                match checking_annotation_scheme with
                | Some annotation ->
                    let expected_ty = instantiate_rigid_scheme state annotation in
                    let _ = infer_expr_against state env binding.value_id expected_ty in
                    expected_ty
                | None -> infer_expr state env binding.value_id
              in
              let bound_entries = bind_pattern state env binding.pattern_id value_ty in
              (
                (binding, export_annotation_scheme, bound_entries),
                solver_group_for_entries state binding.value_id value_ty bound_entries.entries
              ))
            bindings
        in
        (inferred_bindings |> List.map fst, inferred_bindings |> List.map snd))
  in
  let generalized_bindings =
    List.map2
      (fun (binding, annotation_scheme, entries) schemes ->
        let schemes = exported_schemes_for_binding annotation_scheme binding entries.entries schemes in
        let generalized_entries =
          List.map2
            (fun entry scheme ->
              Binding.with_scheme (canonicalize_scheme_in_env state env scheme) entry)
            entries.entries
            schemes
        in
        (binding, { entries with entries = generalized_entries }))
      inferred_bindings
      generalized_groups
  in
  List.fold_left (fun env (_, entries) -> env_with_pattern_bindings env entries) env generalized_bindings

and infer_recursive_group = fun (state: state) env bindings ->
  let unsupported_bindings =
    List.filter
      (fun (binding: BodyArena.binding) -> not (is_recursive_binding_supported state binding))
      bindings
  in
  if List.is_empty unsupported_bindings then
    let (placeholder_info, generalized_groups) =
      Solver.with_local_level_gen state.solver ~variance_of_named:(variances_for_named_type state)
        (fun () ->
          let placeholder_info =
            bindings
            |> List.filter_map
              (fun (binding: BodyArena.binding) ->
                let export_annotation_scheme = binding.annotation
                |> Option.map (canonicalize_generalized_scheme_in_env state env) in
                let checking_annotation_scheme = binding.annotation
                |> Option.map (canonicalize_scheme_in_env state env) in
                let placeholder_ty =
                  match checking_annotation_scheme with
                  | Some annotation -> instantiate_rigid_scheme state annotation
                  | None -> fresh_var state
                in
                match binding.name with
                | Some name ->
                    let entry =
                      match export_annotation_scheme with
                      | Some annotation ->
                          pattern_binding state binding.pattern_id ~name ~scheme:annotation
                      | None ->
                          generalized_pattern_binding state binding.pattern_id ~name placeholder_ty
                    in
                    Some (
                      binding,
                      export_annotation_scheme,
                      Option.is_some checking_annotation_scheme,
                      placeholder_ty,
                      entry
                    )
                | None -> None)
          in
          let placeholders = placeholder_info |> List.map (fun (_, _, _, _, entry) -> entry) in
          let provisional_env = Env.extend env placeholders in
          let () =
            List.iter
              (fun ((binding: BodyArena.binding), _annotation_scheme, has_annotation, placeholder_ty, _entry) ->
                if has_annotation then
                  let _ = infer_expr_against state provisional_env binding.value_id placeholder_ty in
                  ()
                else
                  let value_ty = infer_expr state provisional_env binding.value_id in
                  try_unify state ~origin:(origin_of_binding state binding) placeholder_ty value_ty)
              placeholder_info
          in
          let groups = placeholder_info
          |> List.map
            (fun ((binding: BodyArena.binding), _annotation_scheme, _has_annotation, placeholder_ty, entry) ->
              solver_group_for_entries state binding.value_id placeholder_ty [ entry ]) in
          (placeholder_info, groups))
    in
    let generalized =
      let rec loop acc placeholder_info generalized_groups =
        match (placeholder_info, generalized_groups) with
        | ((binding, annotation_scheme, _has_annotation, _placeholder_ty, entry) :: rest_bindings, schemes :: rest_groups) ->
            let schemes = exported_schemes_for_binding annotation_scheme binding [ entry ] schemes in
            let entry =
              match schemes with
              | [ scheme ] -> Binding.with_scheme (canonicalize_scheme_in_env state env scheme) entry
              | _ -> entry
            in
            loop (entry :: acc) rest_bindings rest_groups
        | _ -> List.rev acc
      in
      loop [] placeholder_info generalized_groups
    in
    Env.extend env generalized
  else
    let () =
      List.iter
        (fun (binding: BodyArena.binding) ->
          add_diagnostic
            state
            (Typ_diagnostic.RecursiveGroupRequiresSimpleVariableBinders {
              binding_span = diagnostic_span (origin_of_binding state binding)
            }))
        unsupported_bindings
    in
    List.fold_left
      (fun env (binding: BodyArena.binding) ->
        let placeholder = fresh_hole state in
        let bound_entries = bind_pattern state env binding.pattern_id placeholder in
        env_with_pattern_bindings env bound_entries)
      env
      bindings

let infer_file = fun ?initial_env ~config ~(source:Source.t) file ->
  let state = make_state ~config file in
  let initial_env =
    match initial_env with
    | Some initial_env -> initial_env
    | None -> Env.bind
      (Env.bind (prelude_env state config) (ambient_env state config))
      (ambient_type_env state config)
  in
  let initial_scope = source.implicit_opens
  |> List.fold_left
    (fun scope module_path -> Env.register_open scope ~scope_path:IdentPath.empty ~module_path)
    Env.empty_item_scope in
  let rec loop export_state type_decls scope = function
    | [] -> (export_state, type_decls, scope)
    | item :: rest -> (
        match item with
        | ItemTree.Type type_item ->
            let item_env = Env.bind
              (Env.for_item_scope export_state scope ~scope_path:type_item.scope_path)
              (type_item_env state type_item) in
            let introduced_type_decls = [
              canonicalize_type_decl_in_env
                state
                item_env
                {
                  FileSummary.scope_path = type_item.scope_path;
                  declaration = type_item.declaration
                }
            ] in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let type_decls = set_visible_type_decls state type_decls in
            let introduced_type_decl =
              type_decls
              |> List.find_map
                (fun (candidate: FileSummary.type_decl) ->
                  if
                    TypeConstructorId.equal
                      candidate.declaration.type_constructor_id
                      type_item.declaration.type_constructor_id
                  then
                    Some candidate
                  else
                    None)
              |> Option.unwrap_or ~default:(List.hd introduced_type_decls)
            in
            let introduced = Env.of_type_decls
              [ { introduced_type_decl with scope_path = IdentPath.empty } ] in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty type_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:type_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:type_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = type_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Exception exception_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:exception_item.scope_path in
            let introduced = exception_bindings state item_env exception_item in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty exception_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:exception_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:exception_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = exception_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.ExtensionConstructor extension_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:extension_item.scope_path in
            let introduced = extension_constructor_bindings state item_env extension_item in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty extension_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:extension_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:extension_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = extension_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Value value_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:value_item.scope_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let env_after_item = infer_binding_group state item_env value_item.binding_ids in
            let introduced = Env.introduced_entries item_env env_after_item in
            let (export_state, scope) =
              if IdentPath.is_empty value_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:value_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:value_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = value_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.DeclaredValue declared_value_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:declared_value_item.scope_path in
            let introduced = declared_value_bindings state item_env declared_value_item in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty declared_value_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:declared_value_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:declared_value_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  {
                    Check_result.item_id = declared_value_item.item_id;
                    binding_names;
                    exports_after
                  }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Open open_item ->
            let scope = Env.register_open
              scope
              ~scope_path:open_item.scope_path
              ~module_path:open_item.module_path in
            let () =
              if state.config.capture_traces then
                let exports_after = Env.export config export_state |> Env.render in
                state.item_traces <- (
                  { Check_result.item_id = open_item.item_id; binding_names = []; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Include include_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:include_item.scope_path in
            let module_path = resolve_module_path_in_scope
              item_env
              state.visible_types
              include_item.scope_path
              include_item.module_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let introduced = Env.entries_for_include item_env module_path in
            let introduced_type_decls = type_decls_for_include state.visible_types module_path
            |> prefix_type_decls include_item.scope_path in
            let (export_state, scope) =
              if IdentPath.is_empty include_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:include_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:include_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = include_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let type_decls = set_visible_type_decls state type_decls in
            loop export_state type_decls scope rest
        | ItemTree.ModuleAlias module_alias_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:module_alias_item.scope_path in
            let module_path = resolve_module_path_in_scope
              item_env
              state.visible_types
              module_alias_item.scope_path
              module_alias_item.module_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export_with_forced_names
                  ~config:state.config
                  ~forced_export_names:state.forced_export_names
                  export_state)
              else
                None
            in
            let alias_export_names = Env.export_names_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path in
            let introduced = Env.entries_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path in
            let introduced_type_decls = type_decls_for_module_alias
              state.visible_types
              ~alias_name:module_alias_item.alias_name
              ~module_path
            |> prefix_type_decls module_alias_item.scope_path in
            let (export_state, scope) =
              if IdentPath.is_empty module_alias_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:module_alias_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:module_alias_item.scope_path introduced
                )
            in
            let forced_export_names =
              if IdentPath.is_empty module_alias_item.scope_path then
                alias_export_names
              else
                List.map
                  (fun name -> qualify_name module_alias_item.scope_path name |> IdentPath.to_string)
                  alias_export_names
            in
            let () =
              state.forced_export_names <- forced_export_names @ state.forced_export_names
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export_with_forced_names
                  ~config:state.config
                  ~forced_export_names:state.forced_export_names
                  export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = module_alias_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let type_decls = set_visible_type_decls state type_decls in
            loop export_state type_decls scope rest
        | ItemTree.Unsupported unsupported_item ->
            let () =
              if state.config.capture_traces then
                let exports_after = Env.export config export_state |> Env.render in
                state.item_traces <- (
                  {
                    Check_result.item_id = unsupported_item.item_id;
                    binding_names = [];
                    exports_after
                  }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
      )
  in
  let (exports, type_decls, scope) = loop
    initial_env
    []
    initial_scope
    (ItemTree.items file.item_tree) in
  let final_env =
    Env.for_item_scope exports scope ~scope_path:IdentPath.empty
    |> fun env ->
      Env.bind env (Env.of_type_decls type_decls)
  in
  let export_env = Env.export_with_forced_names
    ~config:state.config
    ~forced_export_names:state.forced_export_names
    exports in
  let annotated_export_overrides =
    ItemTree.items file.item_tree
    |> List.fold_left
      (fun acc item ->
        match item with
        | ItemTree.Value { scope_path; binding_ids; _ } when IdentPath.is_empty scope_path ->
            binding_ids |> List.fold_left
              (fun acc binding_id ->
                match SemanticTree.find_binding state.file binding_id with
                | Some ({ name=Some name; annotation=Some annotation; _ }: BodyArena.binding) -> (
                  name,
                  canonicalize_generalized_scheme_in_env state final_env annotation
                )
                :: acc
                | _ -> acc)
              acc
        | _ -> acc)
      []
  in
  let export_bindings = export_binding_refs export_env in
  let exports =
    export_env
    |> Env.render
    |> List.map
      (fun (name, scheme) ->
        let scheme =
          match List.assoc_opt name annotated_export_overrides with
          | Some annotation_scheme -> annotation_scheme
          | None -> canonicalize_generalized_scheme_in_env state final_env scheme
        in
        (name, scheme))
  in
  {
    exports;
    export_bindings;
    type_decls;
    item_traces = List.rev state.item_traces;
    expr_traces = List.rev state.expr_traces;
    diagnostics = List.rev state.diagnostics;
  }
