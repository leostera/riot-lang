open Std
open Analysis
open Diagnostics
open Model

type variance =
  | Covariant
  | Contravariant
  | Invariant

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
  declaration_variances: (TypeConstructorId.t, variance list) Collections.HashMap.t;
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

let reset_declaration_variances = fun (state: t) -> Collections.HashMap.clear state.declaration_variances

let make = fun ~config file ->
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
    visible_type_decls = config.ambient_type_decls;
    visible_type_decl_by_path = Collections.HashMap.with_capacity 32;
    visible_type_decl_by_id = Collections.HashMap.with_capacity 32;
    declaration_variances = Collections.HashMap.with_capacity 32;
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
    state.visible_type_decls <- bind_type_decls state.config.ambient_type_decls type_decls
  in
  let () = rebuild_visible_type_decl_indexes state in
  reset_declaration_variances state
