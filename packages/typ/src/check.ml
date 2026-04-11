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
      requirements: MissingRequirements.t
    }
  | MissingModuleTypings of { module_name: LocalModules.InternalName.t }
  | MissingAnalysis of { module_name: LocalModules.InternalName.t; path: Path.t }
  | StoreFailure of { module_name: LocalModules.InternalName.t; reason: string }
  | PackageStoreFailure of { package_name: string; reason: string }

let check = fun ~config ~(source: Source.t) ->
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
      (match source.origin with
      | Source.Path path -> path
      | Source.Label label -> Path.v label);
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

type module_id = int

type dependency_set_id = int

type visible_module_name =
  | InternalName of LocalModules.InternalName.t
  | AmbientName of LocalModules.AmbientName.t

type graph_source = {
  prepared: prepared_source;
  required_names: LocalModules.RequiredName.t array;
  dependency_set_id: dependency_set_id;
  missing_external_names: LocalModules.RequiredName.t array;
}

type graph_group = {
  id: module_id;
  internal_name: LocalModules.InternalName.t;
  visible_names: visible_module_name array;
  sources: graph_source list;
  public_module_names: LocalModules.AmbientName.t array;
  dependency_ids: module_id array;
}

type package_graph = {
  groups: graph_group array;
  candidate_ids_by_required_name:
    (LocalModules.RequiredName.t, module_id array) Collections.HashMap.t;
  dependency_local_ids_by_set_id: module_id array array;
}

type engine_state = {
  package_env: PackageEnv.t;
  loaded_modules: LoadedModules.t;
  compiled_modules_by_id: finished_group option array;
}

let dedupe_module_ids_preserving_order = fun module_ids ->
  let seen = Collections.HashSet.with_capacity (List.length module_ids + 1) in
  module_ids |> List.filter
    (fun module_id ->
      if Collections.HashSet.contains seen module_id then
        false
      else
        let _ = Collections.HashSet.insert seen module_id in
        true)

let dedupe_by_key_preserving_order = fun ~key items ->
  let seen = Collections.HashSet.with_capacity (List.length items + 1) in
  items |> List.filter
    (fun item ->
      let item_key = key item in
      if Collections.HashSet.contains seen item_key then
        false
      else
        let _ = Collections.HashSet.insert seen item_key in
        true)

let visible_module_name_to_string = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> LocalModules.InternalName.to_string internal_name
  | AmbientName ambient_name -> LocalModules.AmbientName.to_string ambient_name

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
    export_status = export_status_of_file_summary (ModuleTypings.to_file_summary
      ~source_id:(List.hd pairing.analyses_by_source |> fst)
      pairing.module_result);
    export_count = List.length (ModuleTypings.exports pairing.module_result);
    type_decl_count = List.length (ModuleTypings.type_decls pairing.module_result);
    mismatch_count = List.length pairing.signature_mismatches;
    mismatch_subjects = List.map Diagnostic.signature_mismatch_name pairing.signature_mismatches;
    mismatch_messages = List.map Diagnostic.signature_mismatch_message pairing.signature_mismatches;
  }

let required_name_of_visible_module_name = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> LocalModules.RequiredName.of_internal_name internal_name
  | AmbientName ambient_name -> LocalModules.RequiredName.of_ambient_name ambient_name

let ambient_names_of_source = fun (source: prepared_source) ->
  dedupe_by_key_preserving_order ~key:(fun ambient_name -> ambient_name)
    (
      LocalModules.local_module_aliases_of_internal_name source.internal_module_name
      @ [ source.local_module_name ]
      @ (
        match source.public_module_name with
        | Some public_name -> [ public_name ]
        | None -> []
      )
    )

let visible_names_of_source = fun (source: prepared_source) ->
  dedupe_by_key_preserving_order
    ~key:(fun visible_name -> visible_name)
    (InternalName source.internal_module_name
    :: (ambient_names_of_source source |> List.map (fun ambient_name -> AmbientName ambient_name)))

let grouped_sources_by_internal_module = fun ordered_sources ->
  let module_order_rev = ref [] in
  let sources_by_module_name = Collections.HashMap.with_capacity 64 in
  ordered_sources |> List.iter
    (fun (source: prepared_source) ->
      let module_name = source.internal_module_name in
      let existing_sources_rev =
        match Collections.HashMap.get sources_by_module_name module_name with
        | Some existing_sources_rev -> existing_sources_rev
        | None ->
            module_order_rev := module_name :: !module_order_rev;
            []
      in
      let _ = Collections.HashMap.insert
        sources_by_module_name
        module_name
        (source :: existing_sources_rev) in
      ());
  !module_order_rev
  |> List.rev
  |> List.filter_map
    (fun module_name ->
      Collections.HashMap.get sources_by_module_name module_name
      |> Option.map (fun sources_rev -> (module_name, List.rev sources_rev)))

let required_names_of_source = fun (source: prepared_source) ->
  let explicit_dependencies =
    match Syn.Deps.of_parse_result source.source.parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let implicit_opens = source.source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter
    (fun module_name ->
      LocalModules.should_include_implicit_open ~current_module_name:source.internal_module_name ~module_name) in
  dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)
    ((explicit_dependencies @ implicit_opens) |> List.map LocalModules.RequiredName.of_string)

let visible_name_arrays_for_group = fun internal_name sources ->
  let visible_names = sources
  |> List.concat_map visible_names_of_source
  |> dedupe_by_key_preserving_order ~key:(fun visible_name -> visible_name) in
  let public_module_names = sources
  |> List.filter_map (fun (source: prepared_source) -> source.public_module_name)
  |> dedupe_by_key_preserving_order ~key:(fun ambient_name -> ambient_name) in
  (Array.of_list visible_names, Array.of_list public_module_names)

let candidate_ids_by_required_name = fun groups ->
  let by_name_rev = Collections.HashMap.with_capacity (Array.length groups * 4) in
  groups |> Array.iter
    (fun (group: graph_group) ->
      group.visible_names |> Array.iter
        (fun visible_name ->
          let required_name = required_name_of_visible_module_name visible_name in
          let existing_rev =
            match Collections.HashMap.get by_name_rev required_name with
            | Some existing_rev -> existing_rev
            | None -> []
          in
          let _ = Collections.HashMap.insert by_name_rev required_name (group.id :: existing_rev) in
          ()));
  let by_name = Collections.HashMap.with_capacity (Array.length groups * 4) in
  Collections.HashMap.iter
    (fun required_name module_ids_rev ->
      let _ = Collections.HashMap.insert
        by_name
        required_name
        (Array.of_list (List.rev module_ids_rev)) in
      ())
    by_name_rev;
  by_name

let dependency_local_ids = fun graph dependency_set_id ->
  graph.dependency_local_ids_by_set_id.(dependency_set_id)

let best_matching_local_module_ids = fun graph (group: graph_group) ~required_module_name ->
  let best_depth = ref None in
  let matches_rev = ref [] in
  let candidate_ids =
    match Collections.HashMap.get graph.candidate_ids_by_required_name required_module_name with
    | Some candidate_ids -> candidate_ids
    | None -> [||]
  in
  candidate_ids |> Array.iter
    (fun candidate_id ->
      let candidate_group = graph.groups.(candidate_id) in
      if not (Int.equal candidate_id group.id) then
        match LocalModules.contextual_match_depth
          ~current_module_name:group.internal_name
          ~required_module_name
          ~candidate_module_name:candidate_group.internal_name with
        | None -> ()
        | Some depth ->
            let current_best = Option.unwrap_or ~default:depth !best_depth in
            if Option.is_none !best_depth || depth > current_best then
              (
                best_depth := Some depth;
                matches_rev := [ candidate_group.id ]
              )
            else if Int.equal depth current_best then
              matches_rev := candidate_group.id :: !matches_rev);
  List.rev !matches_rev |> Array.of_list

let resolution_ids_by_required_name = fun graph (group: graph_group) required_names ->
  let by_name = Collections.HashMap.with_capacity (List.length required_names + 1) in
  required_names
  |> dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)
  |> List.iter
    (fun required_name ->
      let local_ids = best_matching_local_module_ids graph group ~required_module_name:required_name in
      let _ = Collections.HashMap.insert by_name required_name local_ids in
      ());
  by_name

let missing_requirements_of_source = fun ~config ~resolution_ids_by_required_name ~(source:prepared_source) ~required_names ->
  let missing_rev = ref [] in
  let required_local_ids_rev = ref [] in
  required_names |> List.iter
    (fun required_module_name ->
      let local_ids =
        match Collections.HashMap.get resolution_ids_by_required_name required_module_name with
        | Some local_ids -> local_ids
        | None -> [||]
      in
      if not (Array.length local_ids = 0) then
        required_local_ids_rev := List.rev_append (Array.to_list local_ids) !required_local_ids_rev
      else if
        not
          (LoadedModules.contains TypConfig.(config.loaded_modules) ~required_name:required_module_name)
      then
        missing_rev := MissingRequirements.MissingModuleSummary {
          module_name = LocalModules.RequiredName.to_string required_module_name;
          requested_by = [ source.source.source_id ]
        }
        :: !missing_rev);
  (
    !required_local_ids_rev |> List.rev |> dedupe_module_ids_preserving_order |> Array.of_list,
    !missing_rev |> List.rev |> MissingRequirements.of_list
  )

let hidden_export_names_of_exports = fun exports ->
  exports
  |> List.map fst
  |> dedupe_by_key_preserving_order ~key:(fun path -> path)

let dedupe_type_decls_preserving_order = fun type_decls ->
  dedupe_by_key_preserving_order ~key:ModuleSurface.type_decl_key type_decls

let visible_module_path = fun visible_name ->
  SurfacePath.of_string (visible_module_name_to_string visible_name)

let implicit_open_required_names_of_source = fun (source: prepared_source) ->
  source.source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter
    (fun module_name ->
      LocalModules.should_include_implicit_open ~current_module_name:source.internal_module_name ~module_name)
  |> List.map LocalModules.RequiredName.of_string
  |> dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)

let scope_view_for_source = fun ~graph ~state ~(group: graph_group) (source: graph_source) ->
  let visible_modules_rev = ref [] in
  let visible_type_decls_rev = ref [] in
  let implicit_open_modules_rev = ref [] in
  let implicit_open_required_names = implicit_open_required_names_of_source source.prepared in
  let required_name_is_implicit_open required_name =
    List.exists (fun candidate -> candidate = required_name) implicit_open_required_names
  in
  let add_visible_module module_id module_name artifact =
    visible_modules_rev := (SurfacePath.of_string module_name, module_id) :: !visible_modules_rev;
    visible_type_decls_rev := List.rev_append
      (ModuleSurface.qualify_type_decls ~module_name (ModuleTypings.type_decls artifact))
      !visible_type_decls_rev
  in
  let add_local_group ~required_name module_id =
    let package_module_id = PackageEnv.ModuleId.Local graph.groups.(module_id).internal_name in
    match PackageEnv.find_artifact state.package_env package_module_id with
    | None -> ()
    | Some artifact ->
        add_visible_module
          package_module_id
          (LocalModules.RequiredName.to_string required_name)
          artifact
  in
  let add_loaded_required_name required_name =
    match PackageEnv.find_loaded state.package_env ~required_name with
    | None -> ()
    | Some artifact ->
        let package_module_id = PackageEnv.ModuleId.Loaded required_name in
        add_visible_module package_module_id (LocalModules.RequiredName.to_string required_name) artifact
  in
  source.required_names |> Array.iter
    (fun required_name ->
      let local_ids = best_matching_local_module_ids graph group ~required_module_name:required_name in
      if Array.length local_ids = 0 then
        add_loaded_required_name required_name
      else (
        local_ids |> Array.iter (add_local_group ~required_name);
        if required_name_is_implicit_open required_name then
          implicit_open_modules_rev := List.rev_append
            (local_ids
            |> Array.to_list
            |> List.map (fun module_id -> PackageEnv.ModuleId.Local graph.groups.(module_id).internal_name))
            !implicit_open_modules_rev
      );
      if required_name_is_implicit_open required_name && Array.length local_ids = 0 then
        implicit_open_modules_rev := PackageEnv.ModuleId.Loaded required_name :: !implicit_open_modules_rev);
  ScopeView.create
    ~visible_modules:(List.rev !visible_modules_rev |> dedupe_by_key_preserving_order ~key:fst)
    ~implicit_open_modules:(List.rev !implicit_open_modules_rev
      |> dedupe_by_key_preserving_order ~key:(fun module_id -> module_id))
    ~visible_type_decls:(List.rev !visible_type_decls_rev |> dedupe_type_decls_preserving_order)

let build_package_graph = fun ~config ~ordered_sources ->
  let grouped_sources = grouped_sources_by_internal_module ordered_sources in
  let grouped_sources_array = grouped_sources |> Array.of_list in
  let groups =
    grouped_sources_array
    |> Array.mapi
      (fun module_id (internal_name, sources) ->
        let visible_names, public_module_names = visible_name_arrays_for_group
          internal_name
          sources in
        {
          id = module_id;
          internal_name;
          visible_names;
          sources = [];
          public_module_names;
          dependency_ids = [||];
        })
  in
  let graph = {
    groups;
    candidate_ids_by_required_name = candidate_ids_by_required_name groups;
    dependency_local_ids_by_set_id = [||]
  } in
  let dependency_set_id_by_local_ids = Collections.HashMap.with_capacity
    (List.length ordered_sources + 1) in
  let next_dependency_set_id = ref 0 in
  let dependency_local_ids_rev = ref [] in
  let intern_dependency_set local_ids =
    let local_ids_key = Array.to_list local_ids in
    match Collections.HashMap.get dependency_set_id_by_local_ids local_ids_key with
    | Some dependency_set_id -> dependency_set_id
    | None ->
        let dependency_set_id = !next_dependency_set_id in
        next_dependency_set_id := dependency_set_id + 1;
        dependency_local_ids_rev := Array.copy local_ids :: !dependency_local_ids_rev;
        let _ = Collections.HashMap.insert dependency_set_id_by_local_ids local_ids_key dependency_set_id in
        dependency_set_id
  in
  let groups =
    Array.mapi
      (fun module_id (group: graph_group) ->
        let (_internal_name, sources) = grouped_sources_array.(module_id) in
        let source_requirements = sources
        |> List.map (fun (source: prepared_source) -> (source, required_names_of_source source)) in
        let resolution_ids_by_required_name = source_requirements
        |> List.concat_map snd
        |> resolution_ids_by_required_name graph group in
        let graph_sources_rev = ref [] in
        let dependency_ids_rev = ref [] in
        source_requirements |> List.iter
          (fun ((source: prepared_source), required_names) ->
            let required_local_ids, missing_requirements = missing_requirements_of_source
              ~config
              ~resolution_ids_by_required_name
              ~source
              ~required_names in
            dependency_ids_rev := List.rev_append (Array.to_list required_local_ids) !dependency_ids_rev;
            let missing_external_names =
              Session.MissingRequirements.requirements missing_requirements
              |> List.filter_map
                (
                  function
                  | MissingRequirements.MissingModuleSummary { module_name; _ } -> Some (LocalModules.RequiredName.of_string
                    module_name)
                  | MissingRequirements.MissingRootSource _
                  | MissingRequirements.LocalModuleCycle _ -> None
                )
              |> Array.of_list
            in
            let dependency_set_id = intern_dependency_set required_local_ids in
            graph_sources_rev := {
              prepared = source;
              required_names = Array.of_list required_names;
              dependency_set_id;
              missing_external_names
            }
            :: !graph_sources_rev);
        let graph_sources = List.rev !graph_sources_rev in
        let dependency_ids = !dependency_ids_rev
        |> List.rev
        |> List.filter (fun dependency_id -> not (Int.equal dependency_id module_id))
        |> dedupe_module_ids_preserving_order
        |> Array.of_list in
        { group with sources = graph_sources; dependency_ids })
      groups
  in
  {
    graph
    with groups;
    dependency_local_ids_by_set_id = !dependency_local_ids_rev |> List.rev |> Array.of_list
  }

let cycle_module_ids = fun path repeated_id ->
  let rec loop seen = function
    | [] -> List.rev (repeated_id :: seen)
    | head :: tail ->
        let seen = head :: seen in
        if Int.equal head repeated_id then
          List.rev seen
        else
          loop seen tail
  in
  loop [] path

let cycle_error = fun graph module_ids ->
  let module_names = module_ids
  |> List.map (fun module_id -> LocalModules.InternalName.to_string graph.groups.(module_id).internal_name) in
  let source_ids = module_ids
  |> List.concat_map
    (fun module_id ->
      graph.groups.(module_id).sources
      |> List.map (fun (source: graph_source) -> source.prepared.source.source_id)) in
  let module_name =
    match module_ids with
    | module_id :: _ -> graph.groups.(module_id).internal_name
    | [] -> panic "cycle_error expected at least one module id"
  in
  let requirements = MissingRequirements.of_list
    [ MissingRequirements.LocalModuleCycle { module_names; source_ids } ] in
  MissingRequirements { module_name; requirements }

let ordered_group_ids = fun graph ->
  let state = Array.make (Array.length graph.groups) 0 in
  let rec visit path ordered module_id =
    match state.(module_id) with
    | 2 ->
        Ok ordered
    | 1 ->
        Error (cycle_error graph (cycle_module_ids path module_id))
    | _ ->
        state.(module_id) <- 1;
        let result =
          graph.groups.(module_id).dependency_ids
          |> Array.fold_left
            (fun result dependency_id ->
              match result with
              | Error _ as err -> err
              | Ok ordered -> visit (module_id :: path) ordered dependency_id)
            (Ok ordered)
        in
        (
          match result with
          | Error _ as err -> err
          | Ok ordered ->
              state.(module_id) <- 2;
              Ok (module_id :: ordered)
        )
  in
  Array.fold_left
    (fun result (group: graph_group) ->
      match result with
      | Error _ as err -> err
      | Ok ordered -> visit [] ordered group.id)
    (Ok [])
    graph.groups |> Result.map List.rev

let persist_module_typings = fun store module_typings ->
  match Store.save_module_typings store module_typings with
  | Ok () -> Ok ()
  | Error reason -> Error reason

let persist_module_views = fun config ~module_name typings ->
  match TypConfig.(config.store) with
  | None -> Ok ()
  | Some store ->
      let rec loop = function
        | [] -> Ok ()
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
  | (_, _, _, []) ->
      Ok ()
  | (None, Some _, _, _) ->
      Ok ()
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
  group.public_module_names
  |> Array.to_list
  |> List.map
    (fun public_name ->
      ModuleSurface.rebind_module_typings
        ~module_name:(LocalModules.AmbientName.to_string public_name)
        module_typings)

let analyze_group = fun ~graph ~state ~config (group: graph_group) ->
  let local_module_names = dependency_local_ids graph
    (match group.sources with
    | source :: _ -> source.dependency_set_id
    | [] -> 0)
  |> Array.to_list
  |> List.map (fun module_id -> LocalModules.InternalName.to_string graph.groups.(module_id).internal_name)
  in
  let analyzed_sources =
    group.sources
    |> List.fold_left
      (fun result (source: graph_source) ->
        match result with
        | Error _ as err -> err
        | Ok analyzed_sources ->
            let unavailable_local_ids = dependency_local_ids graph source.dependency_set_id
            |> Array.to_list
            |> List.filter (fun module_id -> Option.is_none state.compiled_modules_by_id.(module_id)) in
            let missing_requirements =
              (source.missing_external_names
              |> Array.to_list
              |> List.map
                (fun missing_module_name ->
                  MissingRequirements.MissingModuleSummary {
                    module_name = LocalModules.RequiredName.to_string missing_module_name;
                    requested_by = [ source.prepared.source.source_id ]
                  }))
              @ (unavailable_local_ids
              |> List.map
                (fun module_id ->
                  MissingRequirements.MissingModuleSummary {
                    module_name = LocalModules.InternalName.to_string graph.groups.(module_id).internal_name;
                    requested_by = [ source.prepared.source.source_id ]
                  }))
            in
            (match missing_requirements with
            | _ :: _ ->
                Error (MissingRequirements {
                  module_name = group.internal_name;
                  requirements = MissingRequirements.of_list missing_requirements
                })
            | [] ->
                let scope_view = scope_view_for_source ~graph ~state ~group source in
                let source_config = TypConfig.with_loaded_module_index ~loaded_modules:state.loaded_modules config in
                let ambient_type_decl_count = List.length (ScopeView.visible_type_decls scope_view) in
                let () = TypConfig.emit_event source_config
                  (fun () ->
                    Event.SourceAnalysisStarted {
                      source_id = source.prepared.source.source_id;
                      module_name = source.prepared.source.module_name;
                      mode = Event.BaseAnalysis;
                      local_module_names;
                      loaded_module_count = LoadedModules.len state.loaded_modules;
                      ambient_binding_count = 0;
                      ambient_type_decl_count;
                    })
                in
                let analysis = SourceAnalysis.analyze
                  ~package_env:state.package_env
                  ~scope_view
                  ~config:source_config
                  source.prepared.source in
                let () = TypConfig.emit_event source_config
                  (fun () -> source_analysis_finished_event analysis)
                in
                Ok ((
                  source.prepared,
                  analysis,
                  ScopeView.visible_type_decls scope_view
                ) :: analyzed_sources)))
      (Ok [])
  in
  match analyzed_sources with
  | Error _ as err -> err
  | Ok analyzed_sources -> Ok (List.rev analyzed_sources)

let public_module_typings_of_compiled_modules = fun graph compiled_modules_by_id ->
  let public_module_typings = LoadedModules.empty in
  Array.iteri
    (fun module_id (group: graph_group) ->
      match compiled_modules_by_id.(module_id) with
      | Some finished_group ->
          rebind_public_module_views
            group
            finished_group.module_result
          |> List.iter (LoadedModules.add public_module_typings)
      | None -> ())
    graph.groups;
  public_module_typings

let fold_package_sources = fun ?package_name ?package_fingerprint ~config ~ordered_sources ~init ~f () ->
  let graph = build_package_graph ~config ~ordered_sources in
  match ordered_group_ids graph with
  | Error _ as err -> err
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
                    let source_ids = analyzed_sources
                    |> List.map (fun ((source: prepared_source), _analysis, _visible_type_decls) -> source.source.source_id) in
                    let () = TypConfig.emit_event config
                      (fun () -> Event.ModulePairingStarted { module_name; source_ids })
                    in
                    let pairing = analyzed_sources
                    |> List.map
                      (fun ((source: prepared_source), analysis, ambient_type_decls) -> {
                        ModulePairing.source = source.source;
                        analysis;
                        ambient_type_decls;
                      })
                      |> ModulePairing.of_sources ~internal_name:group.internal_name
                    in
                    let () = TypConfig.emit_event config
                      (fun () -> module_pairing_finished_event module_name source_ids pairing)
                    in
                    let checked_sources = analyzed_sources
                    |> List.map
                      (fun ((source: prepared_source), analysis, _ambient_type_decls) -> {
                        path = source.display_path;
                        analysis
                      })
                    in
                    let module_result = pairing.module_result in
                    let public_module_typings = rebind_public_module_views group module_result in
                    let persisted_typings = module_result :: public_module_typings in
                    match persist_module_views config ~module_name:group.internal_name persisted_typings with
                    | Error _ as err -> err
                    | Ok () ->
                        PackageEnv.add_local state.package_env ~internal_name:group.internal_name module_result;
                        LoadedModules.add state.loaded_modules module_result;
                        let finished_group = {
                          module_name = group.internal_name;
                          checked_sources;
                          module_result;
                        } in
                        state.compiled_modules_by_id.(module_id) <- Some finished_group;
                        Ok (f acc finished_group, state))
          (Ok (init, initial_state))
      in
      match result with
      | Error _ as err -> err
      | Ok (acc, state) ->
          let public_module_typings = public_module_typings_of_compiled_modules
            graph
            state.compiled_modules_by_id in
          match persist_package_bundle config ?package_name ?package_fingerprint public_module_typings with
          | Error _ as err -> err
          | Ok () -> Ok {
            acc;
            loaded_modules = state.loaded_modules;
            public_module_typings
          }
