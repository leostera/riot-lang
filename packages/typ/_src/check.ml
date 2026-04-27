open Std
open Analysis
open Model

module Array = Collections.Array

type prepared_source = {
  display_path: Path.t;
  internal_module_name: LocalModules.InternalName.t;
  local_module_name: LocalModules.AmbientName.t;
  public_module_name: LocalModules.AmbientName.t option;
  source: Source.t;
}

type checked_source = {
  path: Path.t;
  analysis: SourceAnalysis.t;
}

type finished_group = {
  module_name: LocalModules.InternalName.t;
  checked_sources: checked_source list;
  module_result: ModuleTypings.t;
}

type 'acc package_check_result = {
  acc: 'acc;
  loaded_modules: LoadedModules.t;
  public_module_typings: LoadedModules.t;
}

type error =
  | MissingRequirements of {
      module_name: LocalModules.InternalName.t;
      requirements: MissingRequirements.t;
    }
  | MissingModuleTypings of {
      module_name: LocalModules.InternalName.t;
    }
  | MissingAnalysis of {
      module_name: LocalModules.InternalName.t;
      path: Path.t;
    }
  | StoreFailure of {
      module_name: LocalModules.InternalName.t;
      reason: string;
    }
  | PackageStoreFailure of { package_name: string; reason: string }

type graph_source = prepared_source LocalModuleGraph.graph_source

type graph_group = prepared_source LocalModuleGraph.group

type package_graph = prepared_source LocalModuleGraph.t

let check = fun ~config ~(source:Source.t) ->
  let analysis = SourceAnalysis.analyze ~config source in
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
    Analysis.Check_result.source_id = source.source_id;
    filename =
      (
        match source.origin with
        | Source.Path path -> path
        | Source.Label label -> Path.v label
      );
    parse_diagnostics = analysis.parse_diagnostics;
    item_tree;
    body_arena;
    origin_map;
    semantic_tree = analysis.semantic_tree;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    file_summary = analysis.file_summary;
    type_index = analysis.type_index;
    exports =
      SourceAnalysis.exports analysis
      |> List.map (fun (name, scheme) -> (SurfacePath.to_string name, scheme));
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }

type engine_state = {
  package_env: PackageEnv.t;
  loaded_modules: LoadedModules.t;
  compiled_modules_by_id: finished_group option array;
}

let dedupe_by_key_preserving_order = fun ~key items ->
  let seen = Collections.HashSet.with_capacity (List.length items + 1) in
  items
  |> List.filter
    (fun item ->
      let item_key = key item in
      if Collections.HashSet.contains seen item_key then
        false
      else
        let _ = Collections.HashSet.insert seen item_key in
        true)

let export_status_of_file_summary = fun (summary: FileSummary.t) ->
  match summary.export_result with
  | FileSummary.TrustedExport _ -> Event.TrustedExport
  | FileSummary.ErroredExport _ -> Event.ErroredExport
  | FileSummary.NoExport -> Event.MissingExport

let source_analysis_finished_event = fun (analysis: SourceAnalysis.t) ->
  Event.SourceAnalysisFinished {
    source_id = analysis.source.source_id;
    module_name = analysis.source.module_name;
    mode = Event.BaseAnalysis;
    parse_diagnostic_count = List.length analysis.parse_diagnostics;
    lowering_diagnostic_count = List.length analysis.lowering_diagnostics;
    typing_diagnostic_count = List.length analysis.typing_diagnostics;
    parse_diagnostics = analysis.parse_diagnostics;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    export_status = export_status_of_file_summary analysis.file_summary;
    export_count = List.length (FileSummary.exports analysis.file_summary);
    type_decl_count = List.length (FileSummary.type_decls analysis.file_summary);
  }

let module_pairing_finished_event = fun module_name source_ids (pairing: ModulePairing.t) ->
  Event.ModulePairingFinished {
    module_name;
    source_ids;
    export_status =
      export_status_of_file_summary
        (
          ModuleTypings.to_file_summary
            ~source_id:(
              List.hd pairing.analyses_by_source
              |> fst
            )
            pairing.module_result
        );
    export_count = List.length (ModuleTypings.exports pairing.module_result);
    type_decl_count = List.length (ModuleTypings.type_decls pairing.module_result);
    mismatch_count = List.length pairing.signature_mismatches;
    mismatch_subjects = List.map Diagnostic.signature_mismatch_name pairing.signature_mismatches;
    mismatch_messages = List.map Diagnostic.signature_mismatch_message pairing.signature_mismatches;
  }

let ambient_names_of_source = fun (source: prepared_source) ->
  dedupe_by_key_preserving_order
    ~key:(fun ambient_name -> ambient_name)
    (
      (LocalModules.local_module_aliases_of_internal_name source.internal_module_name
      @ [ source.local_module_name ]) @ (
        match source.public_module_name with
        | Some public_name -> [ public_name ]
        | None -> []
      )
    )

let hidden_export_names_of_exports = fun exports ->
  exports
  |> List.map fst
  |> dedupe_by_key_preserving_order ~key:(fun path -> path)

let dedupe_type_decls_preserving_order = fun type_decls ->
  dedupe_by_key_preserving_order
    ~key:ModuleSurface.type_decl_key
    type_decls

let visible_module_path = fun visible_name ->
  SurfacePath.of_string
    (LocalModuleGraph.visible_module_name_to_string visible_name)

let implicit_open_required_names_of_source = fun (source: prepared_source) ->
  source.source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter
    (fun module_name ->
      LocalModules.should_include_implicit_open
        ~current_module_name:source.internal_module_name
        ~module_name)
  |> List.map LocalModules.RequiredName.of_string
  |> dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)

let graph_input_of_source = fun (source: prepared_source) ->
  {
    LocalModuleGraph.payload = source;
    source_id = source.source.source_id;
    internal_name = source.internal_module_name;
    visible_names =
      LocalModuleGraph.InternalName source.internal_module_name
      :: (
        ambient_names_of_source source
        |> List.map (fun ambient_name -> LocalModuleGraph.AmbientName ambient_name)
      );
    required_names = LocalModuleGraph.required_names_of_parse_result
      ~current_module_name:source.internal_module_name
      ~parse_result:source.source.parse_result
      ~implicit_opens:source.source.implicit_opens;
  }

let scope_view_for_source = fun
  ~(graph:package_graph)
  ~state
  ~(group:graph_group)
  (source: graph_source) ->
  let visible_modules_rev = ref [] in
  let implicit_open_modules_rev = ref [] in
  let prepared = source.input.payload in
  let implicit_open_required_names = implicit_open_required_names_of_source prepared in
  let required_name_is_implicit_open required_name =
    List.exists (fun candidate -> candidate = required_name) implicit_open_required_names
  in
  let add_visible_module module_id module_name =
    visible_modules_rev := (SurfacePath.of_string module_name, module_id) :: !visible_modules_rev
  in
  let add_implicit_open_module required_name module_id =
    implicit_open_modules_rev := (
      SurfacePath.of_string (LocalModules.RequiredName.to_string required_name),
      module_id
    )
    :: !implicit_open_modules_rev
  in
  let add_local_group ~required_name module_id =
    let package_module_id = PackageEnv.ModuleId.Local graph.groups.(module_id).internal_name in
    match PackageEnv.find_artifact state.package_env package_module_id with
    | None -> ()
    | Some _ ->
        add_visible_module package_module_id (LocalModules.RequiredName.to_string required_name)
  in
  let add_loaded_required_name required_name =
    match PackageEnv.find_loaded state.package_env ~required_name with
    | None -> ()
    | Some _ ->
        let package_module_id = PackageEnv.ModuleId.Loaded required_name in
        add_visible_module package_module_id (LocalModules.RequiredName.to_string required_name)
  in
  source.required_names
  |> Array.iter
    (fun required_name ->
      let local_ids =
        LocalModuleGraph.best_matching_local_module_ids
          graph
          group
          ~required_module_name:required_name
      in
      if Array.length local_ids = 0 then
        add_loaded_required_name required_name
      else (
        local_ids
        |> Array.iter (add_local_group ~required_name);
        if required_name_is_implicit_open required_name then
          implicit_open_modules_rev := List.rev_append
            (
              local_ids
              |> Array.to_list
              |> List.map
                (fun module_id -> (SurfacePath.of_string
                  (LocalModules.RequiredName.to_string required_name), PackageEnv.ModuleId.Local graph.groups.(module_id).internal_name))
            )
            !implicit_open_modules_rev
      );
      if required_name_is_implicit_open required_name && Array.length local_ids = 0 then
        add_implicit_open_module required_name (PackageEnv.ModuleId.Loaded required_name));
  ScopeView.create
    ~visible_modules:(
      List.rev !visible_modules_rev
      |> dedupe_by_key_preserving_order ~key:fst
    )
    ~implicit_open_modules:(
      List.rev !implicit_open_modules_rev
      |> dedupe_by_key_preserving_order ~key:fst
    )

let build_package_graph = fun ~ordered_sources ->
  LocalModuleGraph.create
    ~ordered_sources:(
      ordered_sources
      |> List.map graph_input_of_source
    )

let public_module_names_of_group = fun (group: graph_group) ->
  group.sources
  |> List.filter_map (fun (source: graph_source) -> source.input.payload.public_module_name)
  |> dedupe_by_key_preserving_order ~key:(fun ambient_name -> ambient_name)

let cycle_error = fun (graph: package_graph) (cycle: LocalModuleGraph.cycle) ->
  let module_name =
    match cycle.module_ids with
    | module_id :: _ -> graph.groups.(module_id).internal_name
    | [] -> panic "cycle_error expected at least one module id"
  in
  let requirements =
    MissingRequirements.of_list
      [
        MissingRequirements.LocalModuleCycle {
          module_names = cycle.module_names;
          source_ids = cycle.source_ids;
        };
      ]
  in
  MissingRequirements { module_name; requirements }

let persist_module_typings = fun store module_typings ->
  match Store.save_module_typings store module_typings with
  | Ok () -> Ok ()
  | Error reason -> Error reason

let persist_module_views = fun config ~module_name typings ->
  match TypConfig.(config.store) with
  | None -> Ok ()
  | Some store ->
      let rec loop = function
        | [] ->
            Ok ()
        | typings :: rest -> (
            match persist_module_typings store typings with
            | Ok () -> loop rest
            | Error reason -> Error (StoreFailure { module_name; reason })
          )
      in
      loop typings

let persist_package_bundle = fun config ?package_name ?package_fingerprint public_module_typings ->
  let typings = LoadedModules.values public_module_typings in
  match (TypConfig.(config.store), package_name, package_fingerprint, typings) with
  | (_, None, _, _)
  | (_, _, _, []) -> Ok ()
  | (None, Some _, _, _) -> Ok ()
  | (Some store, Some package_name, Some fingerprint, typings) -> (
      match Store.save_package_bundle store ~package_name ~fingerprint typings with
      | Ok () -> Ok ()
      | Error reason -> Error (PackageStoreFailure { package_name; reason })
    )
  | (Some store, Some package_name, None, typings) -> (
      match Store.save_package_module_typings store ~package_name typings with
      | Ok () -> Ok ()
      | Error reason -> Error (PackageStoreFailure { package_name; reason })
    )

let rebind_public_module_views = fun (group: graph_group) module_typings ->
  public_module_names_of_group group
  |> Array.of_list
  |> Array.to_list
  |> List.map
    (fun public_name ->
      ModuleSurface.rebind_module_typings
        ~module_name:(LocalModules.AmbientName.to_string public_name)
        module_typings)

let analyze_group = fun ~(graph:package_graph) ~state ~config (group: graph_group) ->
  let local_module_names =
    LocalModuleGraph.dependency_local_ids
      graph
      (
        match group.sources with
        | source :: _ -> source.dependency_set_id
        | [] -> 0
      )
    |> Array.to_list
    |> List.map
      (fun module_id -> LocalModules.InternalName.to_string graph.groups.(module_id).internal_name)
  in
  let analyzed_sources =
    group.sources
    |> List.fold_left
      (fun result (source: graph_source) ->
        match result with
        | Error _ as err -> err
        | Ok analyzed_sources ->
            let prepared = source.input.payload in
            let unavailable_local_ids =
              LocalModuleGraph.dependency_local_ids graph source.dependency_set_id
              |> Array.to_list
              |> List.filter
                (fun module_id -> Option.is_none state.compiled_modules_by_id.(module_id))
            in
            let missing_requirements =
              (
                source.unresolved_local_names
                |> Array.to_list
                |> List.filter
                  (fun required_name ->
                    not
                      (LoadedModules.contains state.loaded_modules ~required_name))
                |> List.map
                  (fun missing_module_name ->
                    MissingRequirements.MissingModuleSummary {
                      module_name = LocalModules.RequiredName.to_string missing_module_name;
                      requested_by = [ source.input.source_id ];
                    })
              ) @ (
                unavailable_local_ids
                |> List.map
                  (fun module_id -> MissingRequirements.MissingModuleSummary {
                    module_name = LocalModules.InternalName.to_string
                      graph.groups.(module_id).internal_name;
                    requested_by = [ source.input.source_id ];
                  })
              )
            in
            (
              match missing_requirements with
              | _ :: _ ->
                  Error (MissingRequirements {
                    module_name = group.internal_name;
                    requirements = MissingRequirements.of_list missing_requirements;
                  })
              | [] ->
                  let scope_view = scope_view_for_source ~graph ~state ~group source in
                  let imported_world =
                    ImportedWorld.create ~package_env:state.package_env ~scope_view
                  in
                  let source_config =
                    TypConfig.with_loaded_module_index ~loaded_modules:state.loaded_modules config
                  in
                  let visible_type_decls = ImportedWorld.visible_type_decls imported_world in
                  let ambient_type_decl_count = List.length visible_type_decls in
                  let () =
                    TypConfig.emit_event
                      source_config
                      (fun () ->
                        Event.SourceAnalysisStarted {
                          source_id = prepared.source.source_id;
                          module_name = prepared.source.module_name;
                          mode = Event.BaseAnalysis;
                          local_module_names;
                          loaded_module_count = LoadedModules.len state.loaded_modules;
                          ambient_binding_count = 0;
                          ambient_type_decl_count;
                        })
                  in
                  let analysis =
                    SourceAnalysis.analyze ~imported_world ~config:source_config prepared.source
                  in
                  let () =
                    TypConfig.emit_event
                      source_config
                      (fun () -> source_analysis_finished_event analysis)
                  in
                  Ok ((prepared, analysis, visible_type_decls) :: analyzed_sources)
            ))
      (Ok [])
  in
  match analyzed_sources with
  | Error _ as err -> err
  | Ok analyzed_sources -> Ok (List.rev analyzed_sources)

let public_module_typings_of_compiled_modules = fun (graph: package_graph) compiled_modules_by_id ->
  let public_module_typings = LoadedModules.empty in
  Array.iteri
    (fun module_id (group: graph_group) ->
      match compiled_modules_by_id.(module_id) with
      | Some finished_group ->
          rebind_public_module_views group finished_group.module_result
          |> List.iter (LoadedModules.add public_module_typings)
      | None -> ())
    graph.groups;
  public_module_typings

let fold_package_sources = fun
  ?package_name
  ?package_fingerprint
  ~config
  ~ordered_sources
  ~init
  ~f
  () ->
  let graph = build_package_graph ~ordered_sources in
  match LocalModuleGraph.ordered_group_ids graph with
  | Error cycle -> Error (cycle_error graph cycle)
  | Ok ordered_group_ids ->
      let initial_state = {
        package_env = PackageEnv.of_loaded_modules TypConfig.(config.loaded_modules);
        loaded_modules = LoadedModules.copy TypConfig.(config.loaded_modules);
        compiled_modules_by_id = Array.make (Array.length graph.groups) None;
      }
      in
      let result =
        ordered_group_ids
        |> List.fold_left
          (fun result module_id ->
            match result with
            | Error _ as err -> err
            | Ok (acc, state) ->
                let group = graph.groups.(module_id) in
                match analyze_group ~graph ~state ~config group with
                | Error _ as err -> err
                | Ok analyzed_sources ->
                    let module_name = LocalModules.InternalName.to_string group.internal_name in
                    let source_ids =
                      analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), _analysis, _visible_type_decls) ->
                          source.source.source_id)
                    in
                    let () =
                      TypConfig.emit_event
                        config
                        (fun () -> Event.ModulePairingStarted { module_name; source_ids })
                    in
                    let pairing =
                      analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis, visible_type_decls) -> {
                          ModulePairing.source = source.source;
                          analysis;
                          visible_type_decls;
                        })
                      |> ModulePairing.of_sources ~internal_name:group.internal_name
                    in
                    let () =
                      TypConfig.emit_event
                        config
                        (fun () ->
                          module_pairing_finished_event module_name source_ids pairing)
                    in
                    let checked_sources =
                      analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis, _visible_type_decls) -> {
                          path = source.display_path;
                          analysis;
                        })
                    in
                    let module_result = pairing.module_result in
                    let public_module_typings = rebind_public_module_views group module_result in
                    let persisted_typings = module_result :: public_module_typings in
                    match persist_module_views
                      config
                      ~module_name:group.internal_name
                      persisted_typings with
                    | Error _ as err -> err
                    | Ok () ->
                        PackageEnv.add_local
                          state.package_env
                          ~internal_name:group.internal_name
                          module_result;
                        LoadedModules.add state.loaded_modules module_result;
                        let finished_group = {
                          module_name = group.internal_name;
                          checked_sources;
                          module_result;
                        }
                        in
                        state.compiled_modules_by_id.(module_id) <- Some finished_group;
                        Ok (f acc finished_group, state))
          (Ok (init, initial_state))
      in
      match result with
      | Error _ as err -> err
      | Ok (acc, state) ->
          let public_module_typings =
            public_module_typings_of_compiled_modules graph state.compiled_modules_by_id
          in
          match persist_package_bundle
            config
            ?package_name
            ?package_fingerprint
            public_module_typings with
          | Error _ as err -> err
          | Ok () -> Ok { acc; loaded_modules = state.loaded_modules; public_module_typings }
