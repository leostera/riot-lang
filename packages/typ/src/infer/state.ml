open Std
open Analysis
open Diagnostics
open Model

type record_type_decl = {
  owner_name: IdentPath.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type variance =
  | Covariant
  | Contravariant
  | Invariant

type t = {
  file: SemanticTree.file;
  config: TypConfig.t;
  regions: Region.t;
  mutable next_type_var_id: int;
  mutable next_binding_ident_stamp: int;
  mutable next_hole_id: int;
  mutable diagnostics: Diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
  mutable record_types: record_type_decl list;
  mutable visible_type_decls: FileSummary.type_decl list;
  visible_type_decl_index: (IdentPath.t, FileSummary.type_decl) Collections.HashMap.t;
  declaration_variances: (IdentPath.t, variance list) Collections.HashMap.t;
  mutable forced_export_names: string list;
}

let qualify_name = fun scope_path name ->
  IdentPath.append_name scope_path name

let record_type_of_summary_decl = fun (type_decl: FileSummary.type_decl) ->
  match type_decl.declaration.labels with
  | [] -> None
  | labels -> Some {
    owner_name = qualify_name type_decl.scope_path type_decl.declaration.type_name;
    param_ids = type_decl.declaration.param_ids;
    labels
  }

let unique_record_types = fun record_types ->
  let seen = Collections.HashSet.with_capacity (List.length record_types) in
  let rec loop acc = function
    | [] -> List.rev acc
    | (record_decl: record_type_decl) :: rest ->
        if Collections.HashSet.contains seen record_decl.owner_name then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen record_decl.owner_name |> ignore in
          loop (record_decl :: acc) rest
  in
  loop [] record_types

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

let rebuild_visible_type_decl_index = fun (state: t) ->
  let () = Collections.HashMap.clear state.visible_type_decl_index in
  state.visible_type_decls |> List.iter
    (fun (type_decl: FileSummary.type_decl) ->
      let _ = Collections.HashMap.insert state.visible_type_decl_index (type_decl_key type_decl) type_decl in
      ())

let reset_declaration_variances = fun (state: t) -> Collections.HashMap.clear state.declaration_variances

let make = fun ~config file ->
  let state = {
    file;
    config;
    regions = Region.create ();
    next_type_var_id = 0;
    next_binding_ident_stamp = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
    record_types = config.ambient_type_decls |> List.filter_map record_type_of_summary_decl |> unique_record_types;
    visible_type_decls = config.ambient_type_decls;
    visible_type_decl_index = Collections.HashMap.with_capacity 32;
    declaration_variances = Collections.HashMap.with_capacity 32;
    forced_export_names = [];
  }
  in
  let () = rebuild_visible_type_decl_index state in
  state

let fresh_var = fun (state: t) ->
  let id = state.next_type_var_id in
  let () =
    state.next_type_var_id <- state.next_type_var_id + 1
  in
  Region.fresh_var state.regions id

let fresh_binding_ident_stamp = fun (state: t) ->
  let stamp = state.next_binding_ident_stamp in
  let () =
    state.next_binding_ident_stamp <- state.next_binding_ident_stamp + 1
  in
  stamp

let fresh_hole = fun (state: t) ->
  let hole_id = state.next_hole_id in
  let () =
    state.next_hole_id <- state.next_hole_id + 1
  in
  TypeRepr.Hole hole_id

let set_visible_type_decls = fun (state: t) type_decls ->
  let () =
    state.visible_type_decls <- bind_type_decls state.config.ambient_type_decls type_decls
  in
  let () = rebuild_visible_type_decl_index state in
  reset_declaration_variances state
