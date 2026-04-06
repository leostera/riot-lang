open Std
open Std.Collections
module Check_error = Error
open Riot_model
module Typ_check_result = Typ.Analysis.Check_result
module Typ_diagnostic = Typ.Model.Diagnostic
module Typ_file_summary = Typ.Model.FileSummary
module Typ_module_typings = Typ.Model.ModuleTypings
module Typ_source = Typ.Model.Source
module Typ_source_id = Typ.Model.SourceId
module Typ_snapshot = Typ.Session.Snapshot
module Typ_source_analysis = Typ.Session.SourceAnalysis

type prepared_source =
  | Readable_source of { path: Path.t; source_id: Typ_source_id.t; typ_source: Typ_source.t }
  | Unreadable_source of { path: Path.t; reason: string }

type readable_typ_source = {
  path: Path.t;
  source_id: Typ_source_id.t;
  source: Typ_source.t;
}

type package_typ_source = {
  internal_module_name: string;
  public_module_name: string option;
  display_path: Path.t;
  source_id: Typ_source_id.t;
  source: Typ_source.t;
}

type checked_group = {
  checked_files: State.checked_file list;
  module_typings: Typ_module_typings.t list;
}

let default_registry_name = "pkgs.ml"

let workspace_manifest_path = fun (workspace: Workspace.t) ->
  Path.(workspace.root / Path.v "riot.toml")

let workspace_can_be_prepared = fun (workspace: Workspace.t) ->
  match Fs.exists (workspace_manifest_path workspace) with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let prepare_workspace = fun (workspace: Workspace.t) ->
  if not (workspace_can_be_prepared workspace) then
    Ok workspace
  else
    match Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:default_registry_name () with
    | Error error -> Error (Check_error.RegistryInitializationFailed {
      registry = default_registry_name;
      error
    })
    | Ok registry -> Riot_deps.ensure_workspace
      ~mode:Riot_deps.Dep_solver.Refresh
      ~registry
      ~workspace
      ()
    |> Result.map_error
      (fun error ->
        Check_error.WorkspacePreparationFailed {
          path = workspace.root;
          error = Riot_model.Pm_error.message error
        })

let report_of_analysis = fun path (analysis: Typ_source_analysis.t) ->
  let (item_tree, body_arena, origin_map) =
    match analysis.semantic_tree with
    | Some semantic_tree -> (
      Some semantic_tree.item_tree,
      Some semantic_tree.body_arena,
      Some semantic_tree.origin_map
    )
    | None -> (None, None, None)
  in
  {
    Typ_check_result.source_id = analysis.source.source_id;
    filename = path;
    parse_diagnostics = analysis.parse_diagnostics;
    item_tree;
    body_arena;
    origin_map;
    semantic_tree = analysis.semantic_tree;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    file_summary = analysis.file_summary;
    type_index = analysis.type_index;
    exports = Typ_source_analysis.exports analysis;
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }

let checked_file_of_analysis = fun path (analysis: Typ_source_analysis.t) ->
  let report = report_of_analysis path analysis in
  let diagnostics = Diagnostic.of_report report in
  State.Typed { path; report; diagnostics }

let workspace_package_by_name = fun (workspace: Workspace.t) package_name ->
  workspace.packages |> List.find_opt
    (fun (pkg: Package.t) ->
      String.equal pkg.name package_name)

let package_typ_source_files = fun ?(include_dev = false) (pkg: Package.t) ->
  let scoped_sources =
    if include_dev then
      pkg.sources.src @ pkg.sources.tests @ pkg.sources.examples @ pkg.sources.bench
    else
      pkg.sources.src
  in
  scoped_sources
  |> List.filter Scope.is_supported_source_file
  |> List.map (fun relative -> Path.(pkg.path / relative))
  |> Scope.dedupe_paths

let package_library_typ_source_files = fun (pkg: Package.t) ->
  match pkg.library with
  | None -> package_typ_source_files pkg
  | Some { path=library_path } ->
      let interface_path = Path.(add_extension (remove_extension library_path) ~ext:"mli") in
      let has_interface =
        match Fs.exists interface_path with
        | Ok true -> true
        | Ok false
        | Error _ -> false
      in
      let files =
        if has_interface then
          [ interface_path; library_path ]
        else
          [ library_path ]
      in
      files |> List.filter Scope.is_supported_source_file |> Scope.dedupe_paths

let package_typ_support_source_files = fun (pkg: Package.t) ->
  package_typ_source_files ~include_dev:false pkg

let merge_module_exports = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((name, _) as export) :: tail ->
        if List.mem name seen then
          loop seen acc tail
        else
          loop (name :: seen) (export :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let type_decl_key = fun (type_decl: Typ_file_summary.type_decl) ->
  String.concat "." (type_decl.scope_path @ [ type_decl.declaration.type_name ])

let merge_module_type_decls = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((type_decl: Typ_file_summary.type_decl) as candidate) :: tail ->
        let key = type_decl_key candidate in
        if List.mem key seen then
          loop seen acc tail
        else
          loop (key :: seen) (candidate :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let typings_with_payload = fun template ~source_hash ~exports ~type_decls ->
  let module_name = Typ_module_typings.module_name template in
  match Typ_module_typings.export_result template, exports with
  | Typ_file_summary.TrustedExport _, _ -> Typ_module_typings.trusted
    ~module_name
    ~source_hash
    ~type_decls
    exports
  | Typ_file_summary.ErroredExport _, _ -> Typ_module_typings.errored
    ~module_name
    ~source_hash
    ~type_decls
    exports
  | Typ_file_summary.NoExport, [] -> Typ_module_typings.missing
    ~module_name
    ~source_hash
    ~type_decls
    ()
  | Typ_file_summary.NoExport, _ -> Typ_module_typings.errored
    ~module_name
    ~source_hash
    ~type_decls
    exports

let merge_module_typings = fun preferred fallback ->
  let module_name = Typ_module_typings.module_name preferred in
  let exports = merge_module_exports
    (Typ_module_typings.exports preferred)
    (Typ_module_typings.exports fallback) in
  let type_decls = merge_module_type_decls
    (Typ_module_typings.type_decls preferred)
    (Typ_module_typings.type_decls fallback) in
  let template =
    match Typ_module_typings.export_result preferred, exports with
    | Typ_file_summary.NoExport, _ :: _ -> fallback
    | _ -> preferred
  in
  let source_hash = Typ_module_typings.synthetic_source_hash
    ~module_name
    ~export_result:(Typ_module_typings.export_result template)
    ~type_decls in
  typings_with_payload template ~source_hash ~exports ~type_decls

let merge_loaded_module_typings = fun preferred fallback ->
  let rec loop order merged remaining =
    match remaining with
    | [] ->
        order |> List.rev |> List.filter_map
          (fun module_name ->
            List.assoc_opt module_name merged)
    | summary :: tail ->
        let module_name = Typ_module_typings.module_name summary in
        let order, merged =
          match List.assoc_opt module_name merged with
          | None -> (module_name :: order, (module_name, summary) :: merged)
          | Some existing -> (
            order,
            (module_name, merge_module_typings existing summary) :: List.remove_assoc module_name merged
          )
        in
        loop order merged tail
  in
  loop [] [] (preferred @ fallback)

let workspace_typ_store_root = fun (workspace: Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "typ-cache")

let workspace_typ_store = fun (workspace: Workspace.t) ->
  let contentstore = Contentstore.create
    ~root:(workspace_typ_store_root workspace)
    ~policy:Contentstore.Policy.default
    () in
  Typ.Store.create contentstore ()

let workspace_build_store = fun (workspace: Workspace.t) ~profile ~target ->
  Riot_store.Store.create_for_lane ~workspace ~profile ~target

let resolve_typ_profile = fun ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  let base_profile = Profile.apply_overrides Profile.debug workspace.profile_overrides in
  let profile = Profile.apply_overrides base_profile pkg.compiler.profile_overrides in
  let target_platform = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () |> Build_ctx.target_platform_name in
  match List.assoc_opt target_platform pkg.compiler.target_overrides with
  | Some { Package.profile_override=Some override } -> Profile.apply_override profile override
  | Some { Package.profile_override=None }
  | None -> profile

let workspace_typ_toolchain = fun (workspace: Workspace.t) ->
  Riot_toolchain.init ~config:(Toolchain_config.from_workspace workspace)

let source_origin_path_for_module = fun (mod_: Riot_model.Module.t) ->
  let qualified_name = mod_ |> Riot_model.Module.module_name |> Riot_model.Module_name.qualified_name in
  match Riot_model.Module.kind mod_ with
  | `interface -> Path.v (qualified_name ^ ".mli")
  | `implementation -> Path.v (qualified_name ^ ".ml")

let public_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  let package_namespace = String.capitalize_ascii pkg.name in
  let module_name = Riot_model.Module.module_name mod_ in
  let namespace = module_name |> Riot_model.Module_name.namespace |> Riot_model.Namespace.to_list in
  let simple_name = Riot_model.Module_name.to_string module_name in
  match namespace with
  | [] when String.equal simple_name package_namespace -> Some simple_name
  | [ root ] when String.equal root package_namespace -> Some simple_name
  | _ -> None

let sanitize_module_name = fun name ->
  String.map
    (fun ch ->
      if ch = '-' then
        '_'
      else
        ch)
    name

let module_name_for_path = fun path ->
  path |> Path.remove_extension |> Path.basename |> sanitize_module_name |> String.capitalize_ascii

let source_rank_for_typ_order = fun path ->
  match Path.extension path with
  | Some ".mli" -> 0
  | Some ".ml" -> 1
  | _ -> 2

let package_typ_sources_from_planner = fun ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  match workspace_typ_toolchain workspace with
  | Error _ -> None
  | Ok toolchain ->
      let profile = resolve_typ_profile ~workspace ~pkg in
      let build_ctx = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () in
      let store = workspace_build_store
        workspace
        ~profile:profile.name
        ~target:(Riot_dirs.host_target ()) in
      let input =
        Riot_planner.Module_planner.{
          package = pkg;
          profile;
          ctx = build_ctx;
          toolchain;
          workspace;
          planning_root = Path.v "src";
          depset = [];
          store;
        }
      in
      match Riot_planner.Module_planner.plan_node input with
      | Error _ -> None
      | Ok plan -> (
          let analyzed_modules = map plan.analyzed_modules in
          match Std.Graph.SimpleGraph.topo_sort plan.module_graph with
          | Error _ -> None
          | Ok nodes ->
              nodes |> List.filter_map
                (fun (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
                  match node.value.kind with
                  | Riot_planner.Module_node.ML mod_
                  | Riot_planner.Module_node.MLI mod_ -> (
                      match HashMap.get analyzed_modules node.id with
                      | None -> None
                      | Some analyzed ->
                          let source_id = Typ_source_id.of_int
                            (Std.Graph.SimpleGraph.Node_id.to_int node.id) in
                          let source =
                            Typ_source.make_prepared ~source_id
                              ~kind:((
                                match node.value.file with
                                | Riot_planner.Module_node.Generated _ -> Typ_source.Generated
                                | Riot_planner.Module_node.Concrete _ -> Typ_source.File
                              ))
                              ~origin:(Typ_source.Path (source_origin_path_for_module mod_))
                              ~revision:0
                              ~source_hash:analyzed.source_hash
                              ~parse_result:analyzed.parse_result
                              ~cst:analyzed.cst
                          in
                          Some {
                            internal_module_name = mod_
                            |> Riot_model.Module.module_name
                            |> Riot_model.Module_name.qualified_name;
                            public_module_name = public_module_name_of_module pkg mod_;
                            display_path = analyzed.display_path;
                            source_id;
                            source;
                          }
                    )
                  | Riot_planner.Module_node.C
                  | Riot_planner.Module_node.H
                  | Riot_planner.Module_node.Other _
                  | Riot_planner.Module_node.Root
                  | Riot_planner.Module_node.Native _
                  | Riot_planner.Module_node.Library _
                  | Riot_planner.Module_node.Binary _ -> None) |> Option.some
        )

let qualify_typings_exports = fun module_name exports ->
  List.map (fun (name, scheme) -> (module_name ^ "." ^ name, scheme)) exports

let qualify_typings_type_decls = fun module_name type_decls ->
  List.map
    (fun (type_decl: Typ_file_summary.type_decl) ->
      {
        Typ_file_summary.scope_path = module_name :: type_decl.scope_path;
        declaration = type_decl.declaration
      })
    type_decls

let ambient_env_for_loaded_modules = fun current_module_name loaded_modules ->
  loaded_modules
  |> List.filter
    (fun typings -> not (String.equal current_module_name (Typ_module_typings.module_name typings)))
  |> List.map
    (fun typings ->
      qualify_typings_exports
        (Typ_module_typings.module_name typings)
        (Typ_module_typings.exports typings))
  |> List.flatten

let ambient_type_decls_for_loaded_modules = fun current_module_name loaded_modules ->
  loaded_modules
  |> List.filter
    (fun typings -> not (String.equal current_module_name (Typ_module_typings.module_name typings)))
  |> List.map
    (fun typings ->
      qualify_typings_type_decls
        (Typ_module_typings.module_name typings)
        (Typ_module_typings.type_decls typings))
  |> List.flatten

let readable_typ_source_of_prepared = function
  | Unreadable_source _ -> None
  | Readable_source { path; source_id; typ_source } -> Some { path; source_id; source = typ_source }

let create_typ_session = fun config paths ->
  let rec loop session prepared_sources remaining =
    match remaining with
    | [] -> (session, List.rev prepared_sources)
    | path :: tail -> (
        match Fs.read path with
        | Error err -> loop
          session
          (Unreadable_source { path; reason = IO.error_message err } :: prepared_sources)
          tail
        | Ok source ->
            let parse_result = Syn.parse ~filename:path source in
            let cst = Syn.build_cst parse_result in
            let session, source_id = Typ.Session.create_prepared_source
              session
              ~kind:Typ_source.File
              ~origin:(Typ_source.Path path)
              ~source_hash:(Typ_source.hash_text
                ~kind:Typ_source.File
                ~origin:(Typ_source.Path path)
                ~text:source)
              ~parse_result
              ~cst in
            let typ_source = Typ_source.make_prepared
              ~source_id
              ~kind:Typ_source.File
              ~origin:(Typ_source.Path path)
              ~revision:0
              ~source_hash:(Typ_source.hash_text
                ~kind:Typ_source.File
                ~origin:(Typ_source.Path path)
                ~text:source)
              ~parse_result
              ~cst in
            loop session (Readable_source { path; source_id; typ_source } :: prepared_sources) tail
      )
  in
  loop (Typ.Session.empty ~config) [] paths

let source_id_of_prepared = function
  | Readable_source { source_id; _ } -> Some source_id
  | Unreadable_source _ -> None

let missing_requirements_reason = fun missing -> "missing type requirements"

let local_module_dependencies_for_source = fun local_module_names (source: readable_typ_source) ->
  match Syn.Deps.of_parse_result source.source.parse_result with
  | Ok deps -> Syn.Deps.modules deps
  |> List.filter
    (fun module_name ->
      HashSet.contains local_module_names module_name
      && not (String.equal module_name (module_name_for_path source.path)))
  |> List.sort_uniq String.compare
  | Error _ -> []

let ordered_readable_typ_sources = fun readable_sources ->
  let grouped_by_module =
    readable_sources
    |> List.fold_left
      (fun grouped (source: readable_typ_source) ->
        let module_name = module_name_for_path source.path in
        let existing =
          match List.assoc_opt module_name grouped with
          | Some sources -> sources
          | None -> []
        in
        (module_name, existing @ [ source ]) :: List.remove_assoc module_name grouped)
      []
    |> List.rev
    |> List.map
      (fun (module_name, sources) ->
        let sources =
          List.sort
            (fun (left: readable_typ_source) (right: readable_typ_source) ->
              Int.compare
                (source_rank_for_typ_order left.path)
                (source_rank_for_typ_order right.path))
            sources
        in
        (module_name, sources))
  in
  let module_order = grouped_by_module |> List.map fst in
  let local_module_names = HashSet.of_list module_order in
  let deps_by_module =
    grouped_by_module
    |> List.map
      (fun (module_name, sources) ->
        let dependencies = sources
        |> List.concat_map (local_module_dependencies_for_source local_module_names)
        |> List.sort_uniq String.compare in
        (module_name, dependencies))
  in
  let permanent = HashSet.with_capacity (List.length module_order) in
  let temporary = HashSet.with_capacity (List.length module_order) in
  let rec visit ordered module_name =
    if HashSet.contains permanent module_name then
      ordered
    else if HashSet.contains temporary module_name then
      ordered
    else
      let () = HashSet.insert temporary module_name |> ignore in
      let ordered =
        match List.assoc_opt module_name deps_by_module with
        | Some dependencies -> dependencies |> List.fold_left visit ordered
        | None -> ordered
      in
      let () = HashSet.remove temporary module_name |> ignore in
      let () = HashSet.insert permanent module_name |> ignore in
      module_name :: ordered
  in
  let ordered_modules = module_order |> List.fold_left visit [] |> List.rev in
  ordered_modules |> List.concat_map
    (fun module_name ->
      match List.assoc_opt module_name grouped_by_module with
      | Some sources -> sources
      | None -> [])

let analyze_package_typ_sources_in_order = fun config ordered_sources ->
  let rec loop loaded_modules analyses remaining =
    match remaining with
    | [] -> List.rev analyses
    | (source: package_typ_source) :: rest ->
        let config = config
        |> Typ.Config.with_loaded_modules ~loaded_modules
        |> Typ.Config.with_ambient
          ~ambient:(ambient_env_for_loaded_modules source.internal_module_name loaded_modules)
        |> Typ.Config.with_ambient_type_decls
          ~ambient_type_decls:(ambient_type_decls_for_loaded_modules source.internal_module_name loaded_modules) in
        let analysis = Typ_source_analysis.analyze ~config source.source in
        let typings = Typ_module_typings.of_file_summary
          ~module_name:source.internal_module_name
          ~source_hash:(Typ_source.input_hash source.source)
          analysis.file_summary in
        let () =
          match config.Typ.Config.store with
          | Some store -> ignore (Typ.Store.save_module_typings store typings)
          | None -> ()
        in
        let loaded_modules = merge_loaded_module_typings [ typings ] loaded_modules in
        loop loaded_modules ((source.source_id, analysis) :: analyses) rest
  in
  loop config.Typ.Config.loaded_modules [] ordered_sources

let package_module_typings_of_analyses = fun ordered_sources analyses ->
  analyses |> List.filter_map
    (fun (source_id, analysis) ->
      match
        List.find_opt
          (fun (source: package_typ_source) ->
            Typ_source_id.equal source.source_id source_id)
          ordered_sources
      with
      | None -> None
      | Some { public_module_name=None; _ } -> None
      | Some source -> Some (Typ_module_typings.of_file_summary
        ~module_name:(Option.expect ~msg:"public module name" source.public_module_name)
        ~source_hash:(Typ_source.input_hash source.source)
        analysis.Typ_source_analysis.file_summary)) |> merge_loaded_module_typings []

let load_package_module_typings_from_store = fun store (pkg: Package.t) ->
  Typ.Store.load_package_module_typings store ~package_name:pkg.name

let persist_module_typings = fun store ?package_name typings ->
  typings |> List.iter (fun typings -> ignore (Typ.Store.save_module_typings store typings));
  match package_name with
  | Some package_name -> ignore (Typ.Store.save_package_module_typings store ~package_name typings)
  | None -> ()

let workspace_dependency_packages = fun ~include_dev (workspace: Workspace.t) (pkg: Package.t) ->
  let dependencies =
    if include_dev then
      Package.build_graph_dependencies pkg
    else
      pkg.dependencies
  in
  dependencies
  |> List.filter_map
    (fun (dependency: Package.dependency) -> workspace_package_by_name workspace dependency.name)
  |> List.sort_uniq
    (fun (left: Package.t) (right: Package.t) ->
      String.compare left.name right.name)

let workspace_module_typings_for_package =
  let rec load cache typ_store (workspace: Workspace.t) ?(visiting = []) (pkg: Package.t) =
    match List.assoc_opt pkg.name !cache with
    | Some typings ->
        typings
    | None when List.mem pkg.name visiting ->
        []
    | None ->
        let dependency_typings = workspace_dependency_packages ~include_dev:false workspace pkg
        |> List.concat_map
          (fun dependency_pkg -> load cache typ_store workspace ~visiting:((pkg.name :: visiting)) dependency_pkg) in
        let package_typings =
          match load_package_module_typings_from_store typ_store pkg with
          | Some typings -> typings
          | None ->
              let loaded_modules = merge_loaded_module_typings
                dependency_typings
                Typ.Config.default.loaded_modules in
              let config = Typ.Config.default
              |> Typ.Config.with_loaded_modules ~loaded_modules
              |> Typ.Config.with_store ~store:(Some typ_store)
              |> Typ.Config.with_capture_traces ~capture_traces:false in
              let typings =
                match package_typ_sources_from_planner ~workspace ~pkg with
                | Some ordered_sources -> analyze_package_typ_sources_in_order config ordered_sources
                |> package_module_typings_of_analyses ordered_sources
                | None ->
                    let session_paths = package_typ_support_source_files pkg in
                    let root_paths = package_library_typ_source_files pkg in
                    let session, prepared_sources = create_typ_session config session_paths in
                    prepared_sources |> List.filter_map
                      (
                        function
                        | Readable_source { path; source_id; _ } when List.exists
                          (fun root_path ->
                            Path.equal root_path path)
                          root_paths -> Some (module_name_for_path path, source_id)
                        | Readable_source _
                        | Unreadable_source _ -> None
                      ) |> List.fold_left
                      (fun grouped (module_name, source_id) ->
                        let existing =
                          match List.assoc_opt module_name grouped with
                          | Some source_ids -> source_ids
                          | None -> []
                        in
                        (module_name, existing @ [ source_id ]) :: List.remove_assoc module_name grouped)
                      [] |> List.rev |> List.concat_map
                      (fun (_module_name, roots) ->
                        match Typ.Session.prepare_snapshot session ~roots with
                        | Ok snapshot -> Typ_snapshot.module_typings snapshot
                        | Error _ -> [])
              in
              let () = persist_module_typings typ_store ~package_name:pkg.name typings in
              typings
        in
        let typings = merge_loaded_module_typings package_typings dependency_typings in
        let () =
          cache := (pkg.name, typings) :: !cache
        in
        typings
  in
  load

let typ_config_for_source_group = fun ~workspace ~summary_cache paths ->
  match paths with
  | [] -> Typ.Config.default |> Typ.Config.with_capture_traces ~capture_traces:false
  | path :: _ -> (
      match Workspace.find_package_for_path workspace ~path with
      | None -> Typ.Config.default |> Typ.Config.with_capture_traces ~capture_traces:false
      | Some pkg ->
          let typ_store = workspace_typ_store workspace in
          let dependency_typings = workspace_dependency_packages ~include_dev:true workspace pkg
          |> List.concat_map
            (fun dependency_pkg ->
              workspace_module_typings_for_package summary_cache typ_store workspace dependency_pkg) in
          let loaded_modules = merge_loaded_module_typings dependency_typings Typ.Config.default.loaded_modules in
          Typ.Config.default
          |> Typ.Config.with_loaded_modules ~loaded_modules
          |> Typ.Config.with_store ~store:(Some typ_store)
          |> Typ.Config.with_capture_traces ~capture_traces:false
    )

let path_key = fun path -> Path.normalize path |> Path.to_string

let prepared_source_path = function
  | Readable_source { path; _ }
  | Unreadable_source { path; _ } -> path

let prepared_sources_by_path = fun prepared_sources ->
  prepared_sources
  |> List.fold_left
    (fun prepared_by_path prepared -> (path_key (prepared_source_path prepared), prepared) :: prepared_by_path)
    []

let checked_group_for_session = fun config session prepared_by_path root_paths ->
  let target_prepared_sources =
    root_paths
    |> List.filter_map
      (fun path ->
        List.assoc_opt (path_key path) prepared_by_path)
  in
  let root_source_ids = target_prepared_sources |> List.filter_map source_id_of_prepared in
  match Typ.Session.prepare_snapshot session ~roots:root_source_ids with
  | Error missing ->
      let reason = missing_requirements_reason missing in
      {
        checked_files =
          target_prepared_sources |> List.map
            (
              function
              | Unreadable_source { path; reason } -> State.Unreadable { path; reason }
              | Readable_source { path; _ } -> State.Unreadable { path; reason }
            );
        module_typings = [];
      }
  | Ok snapshot ->
      let () =
        match config.Typ.Config.store with
        | Some store -> persist_module_typings store (Typ_snapshot.module_typings snapshot)
        | None -> ()
      in
      {
        checked_files =
          target_prepared_sources |> List.map
            (
              function
              | Unreadable_source { path; reason } -> State.Unreadable { path; reason }
              | Readable_source { path; source_id; _ } -> (
                  match Typ.Query.analysis_of_source snapshot source_id with
                  | Some analysis -> checked_file_of_analysis path analysis
                  | None ->
                      let reason = "missing type analysis for " ^ Path.to_string path in
                      State.Unreadable { path; reason }
                )
            );
        module_typings = Typ_snapshot.module_typings snapshot;
      }

let package_root_for_target = fun (workspace: Workspace.t) path ->
  workspace.packages |> List.filter Package.is_workspace_member |> List.sort
    (fun (left: Package.t) (right: Package.t) ->
      Int.compare
        (String.length (Path.to_string right.path))
        (String.length (Path.to_string left.path))) |> List.find_opt
    (fun (pkg: Package.t) ->
      Path.equal path pkg.path || match Path.strip_prefix path ~prefix:pkg.path with
      | Ok _ -> true
      | Error _ -> false) |> Option.map (fun (pkg: Package.t) -> pkg.path)

let grouped_targets_for_session = fun ~workspace target_files ->
  target_files |> List.fold_left
    (fun groups path ->
      let key =
        match package_root_for_target workspace path with
        | Some package_root -> Path.to_string package_root
        | None -> Path.to_string (Path.dirname path)
      in
      let existing =
        match List.assoc_opt key groups with
        | Some existing -> existing
        | None -> []
      in
      (key, existing @ [ path ]) :: List.remove_assoc key groups)
    [] |> List.rev

let session_source_paths_for_group = fun ~workspace ~scan_mode group_targets ->
  match scan_mode, group_targets with
  | false, path :: _ -> (
      match Workspace.find_package_for_path workspace ~path with
      | Some pkg -> (
          match package_typ_source_files ~include_dev:true pkg with
          | [] -> group_targets
          | session_paths -> Scope.dedupe_paths (group_targets @ session_paths)
        )
      | None -> group_targets
    )
  | _ -> group_targets

let grouped_root_targets_for_session = fun group_targets ->
  group_targets |> List.fold_left
    (fun groups path ->
      let module_name = module_name_for_path path in
      let existing =
        match List.assoc_opt module_name groups with
        | Some existing -> existing
        | None -> []
      in
      (module_name, existing @ [ path ]) :: List.remove_assoc module_name groups)
    [] |> List.rev

let ordered_grouped_root_targets_for_session = fun prepared_by_path group_targets ->
  let grouped = grouped_root_targets_for_session group_targets in
  let ordered_unique_module_names =
    let rec loop seen ordered remaining =
      match remaining with
      | [] -> List.rev ordered
      | module_name :: rest ->
          if List.mem module_name seen then
            loop seen ordered rest
          else
            loop (module_name :: seen) (module_name :: ordered) rest
    in
    loop [] []
  in
  let ordered_module_names =
    group_targets
    |> List.filter_map
      (fun path ->
        List.assoc_opt (path_key path) prepared_by_path)
    |> List.filter_map readable_typ_source_of_prepared
    |> ordered_readable_typ_sources
    |> List.map (fun source -> module_name_for_path source.path)
    |> ordered_unique_module_names
  in
  let ordered_groups =
    ordered_module_names
    |> List.filter_map
      (fun module_name ->
        grouped |> List.find_opt
          (fun (name, _) ->
            String.equal name module_name))
  in
  let remaining_groups = grouped
  |> List.filter (fun (module_name, _) -> not (List.mem module_name ordered_module_names)) in
  ordered_groups @ remaining_groups

let check_target_files = fun ~workspace ~scan_mode ?on_result target_files ->
  let summary_cache = ref [] in
  let checked_by_path = ref [] in
  let emit checked_file =
    checked_by_path := (path_key (State.checked_file_path checked_file), checked_file) :: !checked_by_path;
    match on_result with
    | Some callback -> callback checked_file
    | None -> ()
  in
  let () =
    grouped_targets_for_session ~workspace target_files
    |> List.iter
      (fun (_, group_targets) ->
        let session_paths = session_source_paths_for_group ~workspace ~scan_mode group_targets in
        let config = typ_config_for_source_group ~workspace ~summary_cache group_targets in
        let session, prepared_sources = create_typ_session config session_paths in
        let prepared_by_path = prepared_sources_by_path prepared_sources in
        let _ =
          ordered_grouped_root_targets_for_session prepared_by_path group_targets
          |> List.fold_left
            (fun (session, config) (_module_name, root_targets) ->
              let checked_group = checked_group_for_session config session prepared_by_path root_targets in
              checked_group.checked_files |> List.iter emit;
              let loaded_modules = merge_loaded_module_typings
                checked_group.module_typings
                config.Typ.Config.loaded_modules in
              let config = Typ.Config.with_loaded_modules config ~loaded_modules in
              let session = Typ.Session.with_config session ~config in
              (session, config))
            (session, config)
        in
        ())
  in
  target_files
  |> List.map
    (fun path ->
      !checked_by_path
      |> List.assoc_opt (path_key path)
      |> Option.expect ~msg:(("missing checked result for " ^ Path.to_string path)))
