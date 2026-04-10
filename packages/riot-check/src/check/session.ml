open Std
open Std.Collections
module Check_event = Event
module Check_error = Error
open Riot_model
module Typ_check_result = Typ.Analysis.Check_result
module Typ_diagnostic = Typ.Model.Diagnostic
module Typ_file_summary = Typ.Model.FileSummary
module Typ_loaded_modules = Typ.Model.LoadedModules
module Typ_module_typings = Typ.Model.ModuleTypings
module Typ_source = Typ.Model.Source
module Typ_source_id = Typ.Model.SourceId
module Typ_visible_types = Typ.Model.VisibleTypes
module Typ_module_pairing = Typ.Session.ModulePairing
module Typ_module_surface = Typ.Session.ModuleSurface
module Typ_snapshot = Typ.Session.Snapshot
module Typ_source_analysis = Typ.Session.SourceAnalysis
module Typ_local_modules = Typ.Session.LocalModules

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
  local_module_name: string;
  public_module_name: string option;
  display_path: Path.t;
  source_id: Typ_source_id.t;
  source: Typ_source.t;
}

type planned_typ_source = {
  generated: bool;
  source: package_typ_source;
}

type checked_group = {
  checked_files: State.checked_file list;
  module_typings: Typ_module_typings.t list;
}

type cached_package_typings = {
  fingerprint: Crypto.hash;
  typings: Typ_loaded_modules.t;
}

type source_group_typ_context = {
  config: Typ.Config.t;
  dependency_packages: cached_package_typings list;
}

type package_scan_ambient_state = {
  loaded_modules: Typ_loaded_modules.t;
  qualified_exports_by_module: (string * Typ.Config.env) list;
  qualified_type_decls_by_module: (string * Typ_file_summary.type_decl list) list;
  qualified_visible_types_by_module: (string * Typ_visible_types.t) list;
  ambient_all: Typ.Config.env;
  ambient_type_decls_all: Typ_file_summary.type_decl list;
  ambient_visible_types_all: Typ_visible_types.t;
}

let emit_event = fun ?on_event event ->
  match on_event with
  | Some callback -> callback event
  | None -> ()

let with_typ_event_sink = fun ?on_event config ->
  match on_event with
  | None -> config
  | Some on_event -> config
  |> Typ.Config.with_on_event ~on_event:(fun event -> on_event (Check_event.Typ { event }))

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
    exports = Typ_source_analysis.exports analysis
    |> List.map (fun (name, scheme) -> (Typ.Model.SurfacePath.to_string name, scheme));
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }

let checked_file_of_analysis = fun path (analysis: Typ_source_analysis.t) ->
  let report = report_of_analysis path analysis in
  let diagnostics = Diagnostic.of_report report in
  State.Typed { path; report; diagnostics }

let typ_check_prepared_source_of_package_source = fun (source: package_typ_source) ->
  {
    Typ.Check.display_path = source.display_path;
    internal_module_name = source.internal_module_name;
    local_module_name = source.local_module_name;
    public_module_name = source.public_module_name;
    source = source.source;
  }

let workspace_package_by_name = fun (workspace: Workspace.t) package_name ->
  workspace.packages |> List.find_opt
    (fun (pkg: Package.t) ->
      String.equal pkg.name package_name)

let package_typ_source_files = fun ?(include_dev = false) (pkg: Package.t) ->
  let sources =
    if
      List.is_empty pkg.sources.src
      && List.is_empty pkg.sources.tests
      && List.is_empty pkg.sources.examples
      && List.is_empty pkg.sources.bench
    then
      Package.scan_sources ~package_path:pkg.path ()
    else
      pkg.sources
  in
  let scoped_sources =
    if include_dev then
      sources.src @ sources.tests @ sources.examples @ sources.bench
    else
      sources.src
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

let planner_package_for_typing = fun (pkg: Package.t) ~(sources:Package.sources) ->
  Package.make
    ~name:pkg.name
    ~path:pkg.path
    ~relative_path:pkg.relative_path
    ~dependencies:pkg.dependencies
    ~dev_dependencies:pkg.dev_dependencies
    ~build_dependencies:pkg.build_dependencies
    ~foreign_dependencies:pkg.foreign_dependencies
    ~binaries:[]
    ?library:pkg.library
    ~sources
    ~compiler:pkg.compiler
    ~commands:pkg.commands
    ~fix_providers:pkg.fix_providers
    ~publish:pkg.publish
    ()

let planner_source_groups_for_package = fun ?(include_dev = false) (pkg: Package.t) ->
  let sources =
    if
      List.is_empty pkg.sources.src
      && List.is_empty pkg.sources.tests
      && List.is_empty pkg.sources.examples
      && List.is_empty pkg.sources.bench
    then
      Package.scan_sources ~package_path:pkg.path ()
    else
      pkg.sources
  in
  let groups = [ (Path.v "src", sources.src); (
      Path.v "tests",
      if include_dev then
        sources.tests
      else
        []
    ); (
      Path.v "examples",
      if include_dev then
        sources.examples
      else
        []
    ); (
      Path.v "bench",
      if include_dev then
        sources.bench
      else
        []
    ); ]
  in
  groups |> List.filter_map
    (fun (planning_root, allowed_source_files) ->
      if List.is_empty allowed_source_files then
        None
      else
        Some (planning_root, allowed_source_files))

let planner_root_mode_for_group = fun (pkg: Package.t) planning_root ->
  if Path.equal planning_root (Path.v "src") then
    Riot_planner.Module_graph.Library_root { library_name = pkg.name }
  else
    Riot_planner.Module_graph.Loose_sources

let merge_module_exports = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((name, _) as export) :: tail ->
        if List.exists (Typ.Model.SurfacePath.equal name) seen then
          loop seen acc tail
        else
          loop (name :: seen) (export :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let type_decl_key = fun (type_decl: Typ_file_summary.type_decl) ->
  if Typ.Model.SurfacePath.is_empty type_decl.scope_path then
    type_decl.declaration.type_name
  else
    Typ.Model.SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name
    |> Typ.Model.SurfacePath.to_string

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
    ~type_decls
    () in
  typings_with_payload template ~source_hash ~exports ~type_decls

let merge_loaded_module_typings = fun preferred fallback ->
  Typ_loaded_modules.merge
    ~preferred:(Typ_loaded_modules.of_list preferred)
    ~fallback:(Typ_loaded_modules.of_list fallback)
    ~combine:merge_module_typings

let merge_loaded_module_index = fun preferred fallback ->
  Typ_loaded_modules.merge ~preferred:(Typ_loaded_modules.of_list preferred) ~fallback ~combine:merge_module_typings

let default_typ_loaded_modules = Typ.Config.default.loaded_modules

let default_typ_config = Typ.Config.default |> Typ.Config.with_loaded_module_index ~loaded_modules:default_typ_loaded_modules

let rebind_module_typings_name = fun ~module_name (typings: Typ_module_typings.t) ->
  let type_decls = Typ_module_surface.qualify_signature_type_decls
    ~module_name
    (Typ_module_typings.type_decls typings) in
  let value_definitions = Typ_module_typings.value_definitions typings in
  match Typ_module_typings.export_result typings with
  | Typ_file_summary.TrustedExport { exports } ->
      let exports = Typ_module_surface.qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash = Typ_module_typings.synthetic_source_hash
        ~module_name
        ~export_result:(Typ_file_summary.TrustedExport { exports })
        ~type_decls
        ~value_definitions
        () in
      Typ_module_typings.trusted ~module_name ~source_hash ~type_decls ~value_definitions exports
  | Typ_file_summary.ErroredExport { exports } ->
      let exports = Typ_module_surface.qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash = Typ_module_typings.synthetic_source_hash
        ~module_name
        ~export_result:(Typ_file_summary.ErroredExport { exports })
        ~type_decls
        ~value_definitions
        () in
      Typ_module_typings.errored ~module_name ~source_hash ~type_decls ~value_definitions exports
  | Typ_file_summary.NoExport ->
      let source_hash = Typ_module_typings.synthetic_source_hash
        ~module_name
        ~export_result:Typ_file_summary.NoExport
        ~type_decls
        ~value_definitions
        () in
      Typ_module_typings.missing ~module_name ~source_hash ~type_decls ~value_definitions ()

let module_typings_for_source_name = fun ~module_name module_typings (source: package_typ_source) ->
  module_typings |> List.find_opt
    (fun typings ->
      String.equal (Typ_module_typings.module_name typings) source.internal_module_name) |> Option.map
    (fun typings ->
      if String.equal module_name source.internal_module_name then
        typings
      else
        rebind_module_typings_name ~module_name typings)

let hash_equal = fun left right ->
  String.equal (Crypto.Digest.hex left) (Crypto.Digest.hex right)

let package_typings_of_cached_packages = fun packages ->
  packages
  |> List.fold_left
    (fun loaded_modules (entry: cached_package_typings) ->
      Typ_loaded_modules.merge ~preferred:loaded_modules ~fallback:entry.typings ~combine:merge_module_typings)
    Typ_loaded_modules.empty

let package_fingerprint_of_typ_sources = fun ordered_sources dependency_packages ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  let () = H.write state "typ-package\x1f" in
  let () =
    dependency_packages
    |> List.iter
      (fun (dependency: cached_package_typings) ->
        H.write state (Crypto.Digest.hex dependency.fingerprint);
        H.write state "\x1f")
  in
  let () =
    ordered_sources
    |> List.iter
      (fun (source: package_typ_source) ->
        H.write state source.internal_module_name;
        H.write state "\x1f";
        H.write state source.local_module_name;
        H.write state "\x1f";
        (
          match source.public_module_name with
          | Some module_name -> H.write state module_name
          | None -> H.write state "-"
        );
        H.write state "\x1f";
        H.write state (Crypto.Digest.hex (Typ_source.input_hash source.source));
        H.write state "\x1f")
  in
  H.finish state

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

let local_module_segments_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  let package_namespace = Package.root_module_name pkg in
  let module_name = Riot_model.Module.module_name mod_ in
  let namespace = module_name |> Riot_model.Module_name.namespace |> Riot_model.Namespace.to_list in
  let simple_name = Riot_model.Module_name.to_string module_name in
  let segments =
    match namespace with
    | root :: rest when String.equal root package_namespace -> rest @ [ simple_name ]
    | _ -> namespace @ [ simple_name ]
  in
  let rec collapse_adjacent_duplicates acc = function
    | [] -> List.rev acc
    | segment :: rest -> (
        match acc with
        | previous :: _ when String.equal previous segment -> collapse_adjacent_duplicates acc rest
        | _ -> collapse_adjacent_duplicates (segment :: acc) rest
      )
  in
  collapse_adjacent_duplicates [] segments

let public_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  match local_module_segments_of_module pkg mod_ with
  | [ module_name ] -> Some module_name
  | _ -> None

let local_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  local_module_segments_of_module pkg mod_ |> String.concat "."

let dedupe_preserving_order = fun names ->
  let seen = Collections.HashSet.with_capacity (List.length names + 1) in
  names |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let local_module_aliases_for_source = fun (source: package_typ_source) ->
  let derived_aliases = Typ_local_modules.local_module_aliases_of_internal_name
    (Typ_local_modules.InternalName.of_string source.internal_module_name)
  |> List.map Typ_local_modules.AmbientName.to_string in
  dedupe_preserving_order
    (
      derived_aliases @ [ source.local_module_name ] @ (
        match source.public_module_name with
        | Some module_name -> [ module_name ]
        | None -> []
      )
    )

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

let package_typ_sources_from_planner = fun ~on_event ~include_dev ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  let () = emit_event
    ?on_event
    (Check_event.PackagePlanningStarted { package_name = pkg.name; include_dev }) in
  match workspace_typ_toolchain workspace with
  | Error _ -> None
  | Ok toolchain ->
      let profile = resolve_typ_profile ~workspace ~pkg in
      let build_ctx = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () in
      let store = workspace_build_store
        workspace
        ~profile:profile.name
        ~target:(Riot_dirs.host_target ()) in
      let next_source_id = ref 0 in
      let sources =
        let effective_sources =
          if
            List.is_empty pkg.sources.src
            && List.is_empty pkg.sources.tests
            && List.is_empty pkg.sources.examples
            && List.is_empty pkg.sources.bench
          then
            Package.scan_sources ~package_path:pkg.path ()
          else
            pkg.sources
        in
        let planner_pkg = planner_package_for_typing pkg ~sources:effective_sources in
        let source_groups = planner_source_groups_for_package ~include_dev pkg in
        let () = emit_event
          ?on_event
          (Check_event.PackagePlanningFinished {
            package_name = pkg.name;
            include_dev;
            group_count = List.length source_groups;
            allowed_source_count = source_groups
            |> List.map
              (fun (_planning_root, allowed_source_files) -> List.length allowed_source_files)
            |> List.fold_left Int.add 0
          }) in
        source_groups |> List.concat_map
          (fun (planning_root, allowed_source_files) ->
            let () = emit_event
              ?on_event
              (Check_event.PackageSourcePreparationStarted {
                package_name = pkg.name;
                planning_root;
                allowed_source_count = List.length allowed_source_files;
                include_dev
              }) in
            let input =
              Riot_planner.Module_planner.{
                package = planner_pkg;
                profile;
                ctx = build_ctx;
                toolchain;
                workspace;
                planning_root;
                allowed_source_files;
                root_mode = planner_root_mode_for_group planner_pkg planning_root;
                depset = [];
                store;
              }
            in
            match Riot_planner.Module_planner.plan_node input with
            | Error err ->
                let () = emit_event
                  ?on_event
                  (Check_event.PackageSourcePreparationFailed {
                    package_name = pkg.name;
                    planning_root;
                    reason = Riot_planner.Planning_error.to_string err
                  }) in
                []
            | Ok plan -> (
                let analyzed_modules = map plan.analyzed_modules in
                match Std.Graph.SimpleGraph.topo_sort plan.module_graph with
                | Error _ ->
                    let () = emit_event
                      ?on_event
                      (Check_event.PackageSourcePreparationFailed {
                        package_name = pkg.name;
                        planning_root;
                        reason = "module graph contains a cycle"
                      }) in
                    []
                | Ok nodes ->
                    let produced_sources =
                      nodes
                      |> List.filter_map
                        (fun (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
                          match node.value.kind with
                          | Riot_planner.Module_node.ML mod_
                          | Riot_planner.Module_node.MLI mod_ -> (
                              match HashMap.get analyzed_modules node.id with
                              | None -> None
                              | Some analyzed -> (
                                  match analyzed.cst with
                                  | Error _ -> None
                                  | Ok cst ->
                                      let source_id = Typ_source_id.of_int !next_source_id in
                                      let () =
                                        next_source_id := !next_source_id + 1
                                      in
                                      let internal_module_name = mod_
                                      |> Riot_model.Module.module_name
                                      |> Riot_model.Module_name.qualified_name in
                                      let source =
                                        let implicit_opens = analyzed.implicit_opens
                                        |> List.map Typ.Model.SurfacePath.of_string in
                                        Typ_source.make_prepared ~source_id
                                          ~kind:(
                                            match node.value.file with
                                            | Riot_planner.Module_node.Generated _ -> Typ_source.Generated
                                            | Riot_planner.Module_node.Concrete _ -> Typ_source.File
                                          )
                                          ~module_name:internal_module_name
                                          ~implicit_opens
                                          ~origin:(Typ_source.Path analyzed.display_path)
                                          ~revision:0
                                          ~source_hash:analyzed.source_hash
                                          ~parse_result:analyzed.parse_result
                                          ~cst
                                      in
                                      Some {
                                        generated =
                                          (
                                            match node.value.file with
                                            | Riot_planner.Module_node.Generated _ -> true
                                            | Riot_planner.Module_node.Concrete _ -> false
                                          );
                                        source =
                                          {
                                            internal_module_name;
                                            local_module_name = local_module_name_of_module pkg mod_;
                                            public_module_name = public_module_name_of_module pkg mod_;
                                            display_path = analyzed.display_path;
                                            source_id;
                                            source;
                                          };
                                      }
                                )
                            )
                          | Riot_planner.Module_node.C
                          | Riot_planner.Module_node.H
                          | Riot_planner.Module_node.Other _
                          | Riot_planner.Module_node.Root
                          | Riot_planner.Module_node.Native _
                          | Riot_planner.Module_node.Library _
                          | Riot_planner.Module_node.Binary _ -> None)
                    in
                    let () = emit_event
                      ?on_event
                      (Check_event.PackageSourcePreparationFinished {
                        package_name = pkg.name;
                        planning_root;
                        produced_source_count = List.length produced_sources;
                        generated_source_count = produced_sources
                        |> List.filter (fun (planned: planned_typ_source) -> planned.generated)
                        |> List.length
                      }) in
                    produced_sources
              ))
      in
      let concrete_module_names =
        sources
        |> List.filter_map
          (fun planned ->
            if planned.generated then
              None
            else
              Some planned.source.internal_module_name)
        |> List.sort_uniq String.compare
      in
      sources
      |> List.filter
        (fun planned ->
          (not planned.generated)
          || not (List.mem planned.source.internal_module_name concrete_module_names))
      |> List.map (fun planned -> planned.source)
      |> Option.some

let qualify_typings_exports = fun module_name type_decls exports ->
  Typ_module_surface.qualify_exports ~module_name ~type_decls exports

let qualify_typings_type_decls = fun module_name type_decls ->
  Typ_module_surface.qualify_type_decls ~module_name type_decls

let package_scan_ambient_empty = {
  loaded_modules = Typ_loaded_modules.empty;
  qualified_exports_by_module = [];
  qualified_type_decls_by_module = [];
  qualified_visible_types_by_module = [];
  ambient_all = [];
  ambient_type_decls_all = [];
  ambient_visible_types_all = Typ_visible_types.empty;
}

let package_scan_ambient_rebuild_all = fun qualified_exports_by_module qualified_type_decls_by_module qualified_visible_types_by_module ->
  (
    qualified_exports_by_module |> List.concat_map snd,
    qualified_type_decls_by_module |> List.concat_map snd,
    qualified_visible_types_by_module |> List.fold_left
      (fun acc (_module_name, visible_types) ->
        Typ_visible_types.merge acc visible_types)
      Typ_visible_types.empty
  )

let package_scan_ambient_with_typings = fun state typings ->
  let module_name = Typ_module_typings.module_name typings in
  let type_decls = Typ_module_typings.type_decls typings in
  let qualified_exports = qualify_typings_exports
    module_name
    type_decls
    (Typ_module_typings.exports typings) in
  let qualified_type_decls = qualify_typings_type_decls module_name type_decls in
  let qualified_visible_types = Typ_visible_types.of_type_decls qualified_type_decls in
  let loaded_modules = Typ_loaded_modules.merge
    ~preferred:(Typ_loaded_modules.of_list [ typings ])
    ~fallback:state.loaded_modules
    ~combine:merge_module_typings in
  match List.assoc_opt module_name state.qualified_exports_by_module, List.assoc_opt
    module_name
    state.qualified_type_decls_by_module, List.assoc_opt module_name state.qualified_visible_types_by_module with
  | None, None, None ->
      {
        loaded_modules;
        qualified_exports_by_module = state.qualified_exports_by_module
        @ [ (module_name, qualified_exports) ];
        qualified_type_decls_by_module = state.qualified_type_decls_by_module
        @ [ (module_name, qualified_type_decls) ];
        qualified_visible_types_by_module = state.qualified_visible_types_by_module
        @ [ (module_name, qualified_visible_types) ];
        ambient_all = state.ambient_all @ qualified_exports;
        ambient_type_decls_all = state.ambient_type_decls_all @ qualified_type_decls;
        ambient_visible_types_all = Typ_visible_types.merge state.ambient_visible_types_all qualified_visible_types;
      }
  | _ ->
      let qualified_exports_by_module = (module_name, qualified_exports)
      :: List.remove_assoc module_name state.qualified_exports_by_module in
      let qualified_type_decls_by_module = (module_name, qualified_type_decls)
      :: List.remove_assoc module_name state.qualified_type_decls_by_module in
      let qualified_visible_types_by_module = (module_name, qualified_visible_types)
      :: List.remove_assoc module_name state.qualified_visible_types_by_module in
      let (ambient_all, ambient_type_decls_all, ambient_visible_types_all) = package_scan_ambient_rebuild_all
        qualified_exports_by_module
        qualified_type_decls_by_module
        qualified_visible_types_by_module in
      {
        loaded_modules;
        qualified_exports_by_module;
        qualified_type_decls_by_module;
        qualified_visible_types_by_module;
        ambient_all;
        ambient_type_decls_all;
        ambient_visible_types_all;
      }

let package_scan_ambient_of_loaded_modules = fun loaded_modules ->
  Typ_loaded_modules.fold
    (fun _module_name typings state -> package_scan_ambient_with_typings state typings)
    loaded_modules
    package_scan_ambient_empty

let relative_module_name = fun ~current_local_module_name module_name ->
  let module_path = Typ.Model.SurfacePath.of_string module_name in
  let current_module_path = Typ.Model.SurfacePath.of_string current_local_module_name in
  let current_scope_path = current_module_path |> Typ.Model.SurfacePath.split_last |> Option.map fst in
  let prefixes = [ current_module_path; (
      match current_scope_path with
      | Some scope_path when not (Typ.Model.SurfacePath.is_empty scope_path) -> scope_path
      | _ -> Typ.Model.SurfacePath.empty
    ); ]
  in
  prefixes |> List.find_map
    (fun prefix ->
      if Typ.Model.SurfacePath.is_empty prefix then
        None
      else
        module_path
        |> Typ.Model.SurfacePath.strip_prefix ~prefix
        |> Option.filter (fun relative -> not (Typ.Model.SurfacePath.is_empty relative))
        |> Option.map Typ.Model.SurfacePath.to_string)

let relative_ambient_exports_for_loaded_modules = fun ?exclude_module_name ~current_local_module_name loaded_modules ->
  Typ_loaded_modules.fold
    (fun module_name typings ambient ->
      if match exclude_module_name with
        | Some excluded_module_name -> String.equal module_name excluded_module_name
        | None -> false then
        ambient
      else
        match relative_module_name ~current_local_module_name module_name with
        | Some relative_module_name -> ambient
        @ qualify_typings_exports
          relative_module_name
          (Typ_module_typings.type_decls typings)
          (Typ_module_typings.exports typings)
        | None -> ambient)
    loaded_modules
    []

let relative_ambient_type_decls_for_loaded_modules = fun ?exclude_module_name ~current_local_module_name loaded_modules ->
  Typ_loaded_modules.fold
    (fun module_name typings ambient ->
      if match exclude_module_name with
        | Some excluded_module_name -> String.equal module_name excluded_module_name
        | None -> false then
        ambient
      else
        match relative_module_name ~current_local_module_name module_name with
        | Some relative_module_name -> ambient
        @ qualify_typings_type_decls relative_module_name (Typ_module_typings.type_decls typings)
        | None -> ambient)
    loaded_modules
    []

let ambient_env_for_loaded_modules = fun ~current_module_name ~current_local_module_name loaded_modules ->
  let base_ambient =
    Typ_loaded_modules.fold
      (fun module_name typings ambient ->
        if String.equal current_module_name module_name then
          ambient
        else
          ambient
          @ qualify_typings_exports
            module_name
            (Typ_module_typings.type_decls typings)
            (Typ_module_typings.exports typings))
      loaded_modules
      []
  in
  base_ambient
  @ relative_ambient_exports_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules

let ambient_type_decls_for_loaded_modules = fun ~current_module_name ~current_local_module_name loaded_modules ->
  let base_ambient =
    Typ_loaded_modules.fold
      (fun module_name typings ambient ->
        if String.equal current_module_name module_name then
          ambient
        else
          ambient @ qualify_typings_type_decls module_name (Typ_module_typings.type_decls typings))
      loaded_modules
      []
  in
  base_ambient
  @ relative_ambient_type_decls_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules

let package_scan_ambient_for_source = fun (state: package_scan_ambient_state) ~(current_module_name:string) ~(current_local_module_name:string) ->
  let (base_ambient, base_ambient_type_decls, base_ambient_visible_types) =
    match List.assoc_opt current_module_name state.qualified_exports_by_module with
    | None -> (state.ambient_all, state.ambient_type_decls_all, state.ambient_visible_types_all)
    | Some _ ->
        let qualified_exports_by_module = state.qualified_exports_by_module
        |> List.filter (fun (module_name, _) -> not (String.equal module_name current_module_name)) in
        let qualified_type_decls_by_module = state.qualified_type_decls_by_module
        |> List.filter (fun (module_name, _) -> not (String.equal module_name current_module_name)) in
        let qualified_visible_types_by_module = state.qualified_visible_types_by_module
        |> List.filter (fun (module_name, _) -> not (String.equal module_name current_module_name)) in
        package_scan_ambient_rebuild_all qualified_exports_by_module qualified_type_decls_by_module qualified_visible_types_by_module
  in
  let loaded_modules = state.loaded_modules in
  let relative_ambient = relative_ambient_exports_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules in
  let relative_ambient_type_decls = relative_ambient_type_decls_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules in
  let ambient_type_decls = base_ambient_type_decls @ relative_ambient_type_decls in
  (
    base_ambient @ relative_ambient,
    ambient_type_decls,
    Typ_visible_types.bind base_ambient_visible_types relative_ambient_type_decls
  )

let readable_typ_source_of_prepared = function
  | Unreadable_source _ -> None
  | Readable_source { path; source_id; typ_source } -> Some { path; source_id; source = typ_source }

let create_typ_session = fun config ordered_sources ->
  let rec loop session prepared_sources remaining =
    match remaining with
    | [] -> (session, List.rev prepared_sources)
    | (source: package_typ_source) :: tail ->
        let session, source_id = Typ.Session.create_source
          session
          ~kind:source.source.kind
          ~module_name:source.internal_module_name
          ~implicit_opens:source.source.implicit_opens
          ~origin:source.source.origin
          ~source_hash:(Typ_source.input_hash source.source)
          ~parse_result:source.source.parse_result
          ~cst:source.source.cst in
        let typ_source = Typ_source.make_prepared
          ~source_id
          ~kind:source.source.kind
          ~module_name:source.internal_module_name
          ~implicit_opens:source.source.implicit_opens
          ~origin:source.source.origin
          ~revision:source.source.revision
          ~source_hash:(Typ_source.input_hash source.source)
          ~parse_result:source.source.parse_result
          ~cst:source.source.cst in
        let session = local_module_aliases_for_source source
        |> List.filter
          (fun module_name -> not (String.equal module_name source.internal_module_name))
        |> List.fold_left
          (fun session module_name -> Typ.Session.register_source_alias session source_id ~module_name)
          session in
        loop
          session
          (Readable_source { path = source.display_path; source_id; typ_source } :: prepared_sources)
          tail
  in
  loop (Typ.Session.empty ~config) [] ordered_sources

let source_id_of_prepared = function
  | Readable_source { source_id; _ } -> Some source_id
  | Unreadable_source _ -> None

let missing_requirements_reason = fun missing ->
  let details =
    Typ.Session.MissingRequirements.requirements missing
    |> List.map
      (
        function
        | Typ.Session.MissingRequirements.MissingRootSource { source_id } -> "root:"
        ^ Int.to_string (Typ.Model.SourceId.to_int source_id)
        | Typ.Session.MissingRequirements.MissingModuleSummary { module_name; _ } -> "module:" ^ module_name
        | Typ.Session.MissingRequirements.LocalModuleCycle { module_names; _ } -> "cycle:"
        ^ String.concat " -> " module_names
      )
    |> String.concat ", "
  in
  if String.equal details "" then
    "missing type requirements"
  else
    "missing type requirements: " ^ details

let incremental_check_error_reason = function
  | Typ.Check.MissingRequirements { module_name; requirements } -> "while checking "
  ^ module_name
  ^ ": "
  ^ missing_requirements_reason requirements
  | Typ.Check.MissingModuleTypings { module_name } -> "missing authoritative module typings for " ^ module_name
  | Typ.Check.MissingAnalysis { module_name; path } -> "missing checked analysis for "
  ^ module_name
  ^ " at "
  ^ Path.to_string path
  | Typ.Check.StoreFailure { module_name; reason } -> "while persisting module typings for "
  ^ module_name
  ^ ": "
  ^ reason

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

let grouped_package_typ_sources_by_internal_module = fun ordered_sources ->
  ordered_sources |> List.fold_left
    (fun grouped (source: package_typ_source) ->
      let existing =
        match List.assoc_opt source.internal_module_name grouped with
        | Some existing -> existing
        | None -> []
      in
      (source.internal_module_name, existing @ [ source ])
      :: List.remove_assoc source.internal_module_name grouped)
    [] |> List.rev

let analyze_package_typ_sources_in_order = fun config ordered_sources ->
  let local_alias_typings_for_sources module_typings sources =
    let rec loop seen acc = function
      | [] -> List.rev acc
      | (source: package_typ_source) :: rest ->
          let aliases = local_module_aliases_for_source source
          |> List.filter
            (fun module_name -> not (String.equal module_name source.internal_module_name))
          |> List.filter (fun module_name -> not (List.mem module_name seen)) in
          loop
            (seen @ aliases)
            (acc
            @ List.map (fun module_name -> rebind_module_typings_name ~module_name module_typings) aliases)
            rest
    in
    loop [] [] sources
  in
  let persist_typings ambient_state source_path typings =
    match config.Typ.Config.store with
    | Some store -> (
        try
          typings |> List.iter
            (fun typings ->
              match Typ.Store.save_module_typings store typings with
              | Ok () -> ()
              | Error message -> raise (Failure message))
        with
        | Failure message ->
            let loaded_module_names = ambient_state.loaded_modules
            |> Typ_loaded_modules.names
            |> List.sort String.compare in
            raise
              (Failure ("while saving module typings for "
              ^ Path.to_string source_path
              ^ ": loaded_modules=["
              ^ String.concat ", " loaded_module_names
              ^ "]: "
              ^ message))
      )
    | None -> ()
  in
  let rec loop ambient_state analyses remaining =
    match remaining with
    | [] -> List.rev analyses
    | (_module_name, sources) :: rest ->
        let analyzed_sources =
          sources
          |> List.map
            (fun (source: package_typ_source) ->
              let (ambient, _ambient_type_decls, ambient_visible_types) = package_scan_ambient_for_source
                ambient_state
                ~current_module_name:source.internal_module_name
                ~current_local_module_name:source.local_module_name in
              let config = config
              |> Typ.Config.with_loaded_module_index ~loaded_modules:ambient_state.loaded_modules
              |> Typ.Config.with_ambient ~ambient
              |> Typ.Config.with_ambient_visible_types ~ambient_visible_types in
              let analysis = Typ_source_analysis.analyze ~config source.source in
              (source, analysis))
        in
        let module_name =
          match sources with
          | source :: _ -> source.internal_module_name
          | [] -> panic "expected non-empty source group"
        in
        let pairing = analyzed_sources
        |> List.map (fun ((source: package_typ_source), analysis) -> (source.source, analysis))
        |> Typ_module_pairing.of_sources ~module_name in
        let local_alias_typings = local_alias_typings_for_sources pairing.module_typings sources in
        let () =
          match sources with
          | source :: _ -> persist_typings
            ambient_state
            source.display_path
            (pairing.module_typings :: local_alias_typings)
          | [] -> ()
        in
        let ambient_state = pairing.module_typings :: local_alias_typings
        |> List.fold_left package_scan_ambient_with_typings ambient_state in
        loop ambient_state (List.rev_append pairing.analyses_by_source analyses) rest
  in
  loop
    (package_scan_ambient_of_loaded_modules config.Typ.Config.loaded_modules)
    []
    (grouped_package_typ_sources_by_internal_module ordered_sources)

let source_analyses_of_analyses = fun ordered_sources analyses ->
  analyses |> List.filter_map
    (fun (source_id, analysis) ->
      ordered_sources |> List.find_opt
        (fun (source: package_typ_source) ->
          Typ_source_id.equal source.source_id source_id) |> Option.map
        (fun source -> (source, analysis)))

let group_source_analyses_by = fun key_of source_analyses ->
  source_analyses |> List.fold_left
    (fun grouped ((source: package_typ_source), analysis) ->
      let key = key_of source in
      let existing =
        match List.assoc_opt key grouped with
        | Some existing -> existing
        | None -> []
      in
      (key, ((source.source, analysis) :: existing)) :: List.remove_assoc key grouped)
    [] |> List.rev |> List.map (fun (key, sources) -> (key, List.rev sources))

let internal_module_results_of_analyses = fun ordered_sources analyses ->
  source_analyses_of_analyses ordered_sources analyses
  |> group_source_analyses_by (fun (source: package_typ_source) -> source.internal_module_name)
  |> List.map
    (fun (module_name, sources) -> (module_name, Typ_module_pairing.of_sources ~module_name sources))

let package_module_typings_of_analyses = fun ordered_sources analyses ->
  let internal_typings_by_module_name =
    internal_module_results_of_analyses ordered_sources analyses
    |> List.fold_left
      (fun by_name (module_name, (result: Typ_module_pairing.t)) ->
        Collections.HashMap.insert by_name module_name result.module_typings |> ignore;
        by_name)
      (Collections.HashMap.with_capacity 64)
  in
  ordered_sources |> List.filter_map
    (fun (source: package_typ_source) ->
      match source.public_module_name with
      | None -> None
      | Some module_name -> Collections.HashMap.get internal_typings_by_module_name source.internal_module_name
      |> Option.map (rebind_module_typings_name ~module_name)) |> fun typings ->
    Typ_loaded_modules.values (merge_loaded_module_typings typings [])

let expected_public_module_names = fun ordered_sources ->
  ordered_sources
  |> List.filter_map (fun (source: package_typ_source) -> source.public_module_name)
  |> List.sort_uniq String.compare

let package_scan_public_module_typings = fun config ordered_sources ->
  analyze_package_typ_sources_in_order config ordered_sources |> package_module_typings_of_analyses ordered_sources

let merge_public_module_typings_with_package_scan = fun config ordered_sources module_typings ->
  match expected_public_module_names ordered_sources with
  | [] -> module_typings
  | _ ->
      let fallback_typings = package_scan_public_module_typings config ordered_sources in
      Typ_loaded_modules.values (merge_loaded_module_typings module_typings fallback_typings)

let recover_missing_public_module_typings = fun config ordered_sources module_typings ->
  let present_public_module_names = module_typings
  |> List.map Typ_module_typings.module_name
  |> List.sort_uniq String.compare in
  let missing_public_module_names = expected_public_module_names ordered_sources
  |> List.filter (fun module_name -> not (List.mem module_name present_public_module_names)) in
  match missing_public_module_names with
  | [] -> module_typings
  | _ -> merge_public_module_typings_with_package_scan config ordered_sources module_typings

let session_path_key = fun path -> Path.normalize path |> Path.to_string

let session_prepared_sources_by_path = fun prepared_sources ->
  prepared_sources |> List.fold_left
    (fun prepared_by_path prepared ->
      let path =
        match prepared with
        | Readable_source { path; _ }
        | Unreadable_source { path; _ } -> path
      in
      (session_path_key path, prepared) :: prepared_by_path)
    []

let session_grouped_root_targets = fun target_paths ->
  target_paths |> List.fold_left
    (fun groups path ->
      let module_name = module_name_for_path path in
      let existing =
        match List.assoc_opt module_name groups with
        | Some existing -> existing
        | None -> []
      in
      (module_name, existing @ [ path ]) :: List.remove_assoc module_name groups)
    [] |> List.rev

let session_ordered_grouped_root_targets = fun prepared_by_path target_paths ->
  let grouped = session_grouped_root_targets target_paths in
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
    target_paths
    |> List.filter_map
      (fun path ->
        List.assoc_opt (session_path_key path) prepared_by_path)
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

(* Keep package bundle construction on the same rooted-session semantics that
   explicit-file checks already use. That avoids package scans inventing a
   different module view than Typ.Session.prepare_snapshot. *)

let checked_group_for_ordered_sources_via_rooted_sessions = fun ?on_event ~package_name ~group_targets config ordered_sources target_paths ->
  let () = emit_event
    ?on_event
    (Check_event.PackageSessionSeedStarted {
      package_name;
      ordered_source_count = List.length ordered_sources;
      target_path_count = List.length target_paths
    }) in
  let session, prepared_sources = create_typ_session config ordered_sources in
  let () = emit_event
    ?on_event
    (Check_event.PackageSessionSeedFinished {
      package_name;
      prepared_source_count = List.length prepared_sources
    }) in
  let prepared_by_path = session_prepared_sources_by_path prepared_sources in
  let sources_by_path = ordered_sources
  |> List.fold_left
    (fun by_path (source: package_typ_source) -> (session_path_key source.display_path, source) :: by_path)
    [] in
  let checked_by_path = ref [] in
  let record checked_file =
    checked_by_path := (session_path_key (State.checked_file_path checked_file), checked_file) :: !checked_by_path
  in
  let record_unreadable path reason = record (State.Unreadable { path; reason }) in
  let () = target_paths
  |> List.filter
    (fun path -> Option.is_none (List.assoc_opt (session_path_key path) prepared_by_path))
  |> List.iter
    (fun path -> record_unreadable path "planner did not produce a prepared CST for this source") in
  let ordered_root_groups =
    if group_targets then
      session_ordered_grouped_root_targets prepared_by_path target_paths
    else
      [ ("__package__", target_paths) ]
  in
  let () = emit_event
    ?on_event
    (Check_event.PackageRootGroupingFinished {
      package_name;
      root_group_count = List.length ordered_root_groups;
      target_path_count = List.length target_paths
    }) in
  let (_, _, module_typings) =
    ordered_root_groups
    |> List.fold_left
      (fun (session, config, accumulated_typings) (_module_name, root_targets) ->
        let target_prepared_sources =
          root_targets
          |> List.filter_map
            (fun path ->
              List.assoc_opt (session_path_key path) prepared_by_path)
        in
        let root_source_ids = target_prepared_sources |> List.filter_map source_id_of_prepared in
        match Typ.Session.prepare_snapshot session ~roots:root_source_ids with
        | Error missing ->
            let reason = missing_requirements_reason missing in
            let () =
              target_prepared_sources
              |> List.iter
                (
                  function
                  | Unreadable_source { path; reason } -> record_unreadable path reason
                  | Readable_source { path; _ } -> record_unreadable path reason
                )
            in
            (session, config, accumulated_typings)
        | Ok snapshot ->
            let rooted_module_typings = Typ_snapshot.module_typings snapshot in
            let () = emit_event
              ?on_event
              (Check_event.PackageSnapshotCheckedFilesStarted {
                package_name;
                root_target_count = List.length root_targets
              }) in
            let () =
              target_prepared_sources
              |> List.iter
                (
                  function
                  | Unreadable_source { path; reason } -> record_unreadable path reason
                  | Readable_source { path; source_id; _ } -> (
                      match Typ.Query.analysis_of_source snapshot source_id with
                      | Some analysis -> record (checked_file_of_analysis path analysis)
                      | None -> record_unreadable
                        path
                        ("missing type analysis for " ^ Path.to_string path)
                    )
                )
            in
            let () = emit_event
              ?on_event
              (Check_event.PackageSnapshotCheckedFilesFinished {
                package_name;
                checked_file_count = List.length target_prepared_sources
              }) in
            let () = emit_event
              ?on_event
              (Check_event.PackageSnapshotReloadStarted {
                package_name;
                root_target_count = List.length root_targets
              }) in
            let module_typings = rooted_module_typings in
            let local_alias_typings =
              root_targets
              |> List.filter_map
                (fun path ->
                  match List.assoc_opt (session_path_key path) sources_by_path with
                  | Some source when not
                    (String.equal source.local_module_name source.internal_module_name) -> module_typings_for_source_name
                    ~module_name:source.local_module_name
                    module_typings
                    source
                  | Some _
                  | None -> None)
            in
            let public_module_typings =
              root_targets
              |> List.filter_map
                (fun path ->
                  match List.assoc_opt (session_path_key path) sources_by_path with
                  | Some source -> (
                      match source.public_module_name with
                      | Some module_name -> module_typings_for_source_name
                        ~module_name
                        module_typings
                        source
                      | None -> None
                    )
                  | None -> None)
            in
            let loaded_modules = merge_loaded_module_index
              (module_typings @ local_alias_typings)
              config.Typ.Config.loaded_modules in
            let () = emit_event ?on_event
              (
                Check_event.PackageSnapshotReloadFinished {
                  package_name;
                  rooted_module_count = List.length module_typings;
                  local_alias_typing_count = List.length local_alias_typings;
                  public_module_typing_count = List.length public_module_typings;
                  loaded_module_count = Typ_loaded_modules.len loaded_modules;
                }
              )
            in
            let config = Typ.Config.with_loaded_module_index config ~loaded_modules in
            let session = Typ.Session.with_config session ~config in
            (
              session,
              config,
              Typ_loaded_modules.values
                (merge_loaded_module_typings public_module_typings accumulated_typings)
            ))
      (session, config, [])
  in
  let () = emit_event
    ?on_event
    (Check_event.PackageCheckedGroupAssembleStarted {
      package_name;
      target_path_count = List.length target_paths
    }) in
  let checked_files =
    target_paths
    |> List.map
      (fun path ->
        match List.assoc_opt (session_path_key path) !checked_by_path with
        | Some checked_file -> checked_file
        | None -> State.Unreadable {
          path;
          reason = "missing checked result for " ^ Path.to_string path
        })
  in
  let () = emit_event
    ?on_event
    (Check_event.PackageCheckedGroupAssembleFinished {
      package_name;
      checked_file_count = List.length checked_files
    }) in
  { checked_files; module_typings }

let load_package_module_typings_from_store = fun store (pkg: Package.t) ->
  match Typ.Store.load_package_bundle store ~package_name:pkg.name with
  | Some ({ typings=[]; _ } as bundle) -> Some {
    fingerprint = bundle.fingerprint;
    typings = Typ_loaded_modules.empty
  }
  | Some bundle -> Some {
    fingerprint = bundle.fingerprint;
    typings = Typ_loaded_modules.of_list bundle.typings
  }
  | None -> None

let save_module_typings_or_raise = fun store typings ->
  match Typ.Store.save_module_typings store typings with
  | Ok () -> ()
  | Error message -> raise (Failure message)

let persist_module_typings = fun store ?package_name ?package_fingerprint typings ->
  let package_bundle_is_current =
    match (package_name, package_fingerprint) with
    | (Some package_name, Some package_fingerprint) -> (
        match Typ.Store.load_package_bundle store ~package_name with
        | Some bundle -> hash_equal bundle.fingerprint package_fingerprint
        | None -> false
      )
    | _ -> false
  in
  if package_bundle_is_current then
    ()
  else (
    typings |> List.iter
      (fun typings ->
        try save_module_typings_or_raise store typings with
        | Failure message -> raise
          (Failure ("while persisting module typings for "
          ^ Typ_module_typings.module_name typings
          ^ ": "
          ^ message)));
    match (package_name, package_fingerprint) with
    | (Some package_name, Some package_fingerprint) when not (List.is_empty typings) -> (
        match Typ.Store.save_package_bundle store ~package_name ~fingerprint:package_fingerprint typings with
        | Ok () -> ()
        | Error message -> raise
          (Failure ("while persisting package bundle for " ^ package_name ^ ": " ^ message))
      )
    | (Some package_name, None) when not (List.is_empty typings) -> (
        match Typ.Store.save_package_module_typings store ~package_name typings with
        | Ok () -> ()
        | Error message -> raise
          (Failure ("while persisting package module typings for " ^ package_name ^ ": " ^ message))
      )
    | _ ->
        ()
  )

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
  let rec load cache typ_store (workspace: Workspace.t) ?on_event ?(visiting = []) (pkg: Package.t) =
    match List.assoc_opt pkg.name !cache with
    | Some entry ->
        entry
    | None when List.mem pkg.name visiting ->
        { fingerprint = Crypto.hash_string ""; typings = Typ_loaded_modules.empty }
    | None ->
        let dependency_packages = workspace_dependency_packages ~include_dev:false workspace pkg
        |> List.map
          (fun dependency_pkg ->
            load cache typ_store workspace ?on_event ~visiting:(pkg.name :: visiting) dependency_pkg) in
        let dependency_typings = package_typings_of_cached_packages dependency_packages in
        let ordered_sources =
          match package_typ_sources_from_planner ~on_event ~include_dev:false ~workspace ~pkg with
          | Some ordered_sources -> ordered_sources
          | None -> []
        in
        let package_fingerprint = package_fingerprint_of_typ_sources ordered_sources dependency_packages in
        let compute_package_typings () =
          let loaded_modules = Typ_loaded_modules.merge
            ~preferred:dependency_typings
            ~fallback:default_typ_loaded_modules
            ~combine:merge_module_typings in
          let config = default_typ_config
          |> Typ.Config.with_loaded_module_index ~loaded_modules
          |> Typ.Config.with_store ~store:(Some typ_store)
          |> Typ.Config.with_capture_traces ~capture_traces:false
          |> with_typ_event_sink ?on_event in
          match Typ.Check.fold_package_sources
            ~config
            ~ordered_sources:(List.map typ_check_prepared_source_of_package_source ordered_sources)
            ~init:Typ_loaded_modules.empty
            ~f:(fun public_typings (finished_group: Typ.Check.finished_group) ->
              Typ_loaded_modules.merge
                ~preferred:(Typ_loaded_modules.of_list finished_group.public_module_typings)
                ~fallback:public_typings
                ~combine:merge_module_typings) with
          | Ok (public_typings, _loaded_modules) -> public_typings
          | Error error -> raise (Failure (incremental_check_error_reason error))
        in
        let package_typings =
          match load_package_module_typings_from_store typ_store pkg with
          | Some cached when hash_equal cached.fingerprint package_fingerprint ->
              let () = emit_event ?on_event (Check_event.PackageCached { package_name = pkg.name }) in
              cached.typings
          | None ->
              let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
              if List.is_empty ordered_sources then
                Typ_loaded_modules.empty
              else
                compute_package_typings ()
          | Some _ ->
              let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
              if List.is_empty ordered_sources then
                Typ_loaded_modules.empty
              else
                compute_package_typings ()
        in
        let typings = Typ_loaded_modules.merge
          ~preferred:package_typings
          ~fallback:dependency_typings
          ~combine:merge_module_typings in
        let entry = { fingerprint = package_fingerprint; typings } in
        let () =
          cache := (pkg.name, entry) :: !cache
        in
        entry
  in
  load

let typ_config_for_source_group = fun ~workspace ~summary_cache ~include_dev ?on_event paths ->
  match paths with
  | [] -> {
    config = default_typ_config
    |> Typ.Config.with_capture_traces ~capture_traces:false
    |> with_typ_event_sink ?on_event;
    dependency_packages = []
  }
  | path :: _ -> (
      match Workspace.find_package_for_path workspace ~path with
      | None -> {
        config = default_typ_config
        |> Typ.Config.with_capture_traces ~capture_traces:false
        |> with_typ_event_sink ?on_event;
        dependency_packages = []
      }
      | Some pkg ->
          let typ_store = workspace_typ_store workspace in
          let dependency_packages = workspace_dependency_packages ~include_dev workspace pkg
          |> List.map
            (fun dependency_pkg ->
              workspace_module_typings_for_package summary_cache typ_store workspace ?on_event dependency_pkg) in
          let dependency_typings = package_typings_of_cached_packages dependency_packages in
          let loaded_modules = Typ_loaded_modules.merge
            ~preferred:dependency_typings
            ~fallback:default_typ_loaded_modules
            ~combine:merge_module_typings in
          {
            config = default_typ_config
            |> Typ.Config.with_loaded_module_index ~loaded_modules
            |> Typ.Config.with_store ~store:(Some typ_store)
            |> Typ.Config.with_capture_traces ~capture_traces:false
            |> with_typ_event_sink ?on_event;
            dependency_packages
          }
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

let analysis_by_source_id = fun analyses ->
  analyses
  |> List.fold_left
    (fun by_source_id (source_id, analysis) -> (Typ_source_id.to_int source_id, analysis) :: by_source_id)
    []

let ordered_sources_by_path = fun ordered_sources ->
  ordered_sources
  |> List.fold_left
    (fun by_path (source: package_typ_source) -> (path_key source.display_path, source) :: by_path)
    []

let checked_group_for_package_scan = fun config ~package_name ~package_fingerprint ordered_sources target_paths ->
  let checked_files_by_path = ref [] in
  let public_module_typings = ref Typ_loaded_modules.empty in
  let record_checked_file path analysis =
    checked_files_by_path := (path_key path, checked_file_of_analysis path analysis) :: !checked_files_by_path
  in
  match
    Typ.Check.fold_package_sources ~config ~ordered_sources:(List.map
      typ_check_prepared_source_of_package_source
      ordered_sources) ~init:()
      ~f:(fun () (finished_group: Typ.Check.finished_group) ->
        finished_group.checked_sources
        |> List.iter
          (fun (checked_source: Typ.Check.checked_source) ->
            record_checked_file checked_source.path checked_source.analysis);
        public_module_typings := Typ_loaded_modules.merge
          ~preferred:(Typ_loaded_modules.of_list finished_group.public_module_typings)
          ~fallback:!public_module_typings
          ~combine:merge_module_typings)
  with
  | Error error ->
      let reason = incremental_check_error_reason error in
      {
        checked_files =
          target_paths |> List.map
            (fun path ->
              match List.assoc_opt (path_key path) !checked_files_by_path with
              | Some checked_file -> checked_file
              | None -> State.Unreadable { path; reason });
        module_typings = Typ_loaded_modules.values !public_module_typings;
      }
  | Ok ((), _loaded_modules) ->
      let checked_group = {
        checked_files =
          target_paths |> List.map
            (fun path ->
              match List.assoc_opt (path_key path) !checked_files_by_path with
              | Some checked_file -> checked_file
              | None -> State.Unreadable {
                path;
                reason = "missing checked result for " ^ Path.to_string path
              });
        module_typings = Typ_loaded_modules.values !public_module_typings;
      }
      in
      let () =
        match config.Typ.Config.store with
        | Some store -> persist_module_typings
          store
          ~package_name
          ~package_fingerprint
          checked_group.module_typings
        | None -> ()
      in
      checked_group

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
  Workspace.find_package_for_path workspace ~path |> Option.map (fun (pkg: Package.t) -> pkg.path)

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

let check_target_files = fun ~workspace ~scan_mode ~include_dev ?on_event ?on_result target_files ->
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
        let { config; dependency_packages } = typ_config_for_source_group
          ~workspace
          ~summary_cache
          ~include_dev
          ?on_event
          group_targets in
        let emit_unreadable reason = group_targets
        |> List.iter (fun path -> emit (State.Unreadable { path; reason })) in
        match group_targets with
        | [] -> ()
        | path :: _ -> (
            match Workspace.find_package_for_path workspace ~path with
            | None -> emit_unreadable "planner-backed source preparation requires a workspace package"
            | Some pkg -> (
                let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
                match package_typ_sources_from_planner ~on_event ~include_dev ~workspace ~pkg with
                | None -> emit_unreadable
                  ("failed to prepare planner-owned sources for package " ^ pkg.name)
                | Some ordered_sources ->
                    let package_fingerprint = package_fingerprint_of_typ_sources ordered_sources dependency_packages in
                    let checked_group = checked_group_for_package_scan
                      config
                      ~package_name:pkg.name
                      ~package_fingerprint
                      ordered_sources
                      group_targets in
                    let () = emit_event
                      ?on_event
                      (Check_event.PackageCheckedGroupEmitStarted {
                        package_name = pkg.name;
                        checked_file_count = List.length checked_group.checked_files
                      }) in
                    let () = checked_group.checked_files |> List.iter emit in
                    let () = emit_event
                      ?on_event
                      (Check_event.PackageCheckedGroupEmitFinished {
                        package_name = pkg.name;
                        checked_file_count = List.length checked_group.checked_files
                      }) in
                    ()
              )
          ))
  in
  target_files
  |> List.map
    (fun path ->
      !checked_by_path
      |> List.assoc_opt (path_key path)
      |> Option.expect ~msg:("missing checked result for " ^ Path.to_string path))
