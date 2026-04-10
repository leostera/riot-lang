open Std
open Analysis
open Model
module Array = Collections.Array

type prepared_source = {
  display_path: Path.t;
  internal_module_name: string;
  local_module_name: string;
  public_module_name: string option;
  source: Source.t;
}

type checked_source = {
  path: Path.t;
  analysis: Session.SourceAnalysis.t;
}

type finished_group = {
  module_name: string;
  checked_sources: checked_source list;
  module_typings: ModuleTypings.t;
  loaded_modules: LoadedModules.t;
}

type 'acc package_check_result = {
  acc: 'acc;
  loaded_modules: LoadedModules.t;
  public_module_typings: LoadedModules.t;
}

type error =
  | MissingRequirements of { module_name: string; requirements: Session.MissingRequirements.t }
  | MissingModuleTypings of { module_name: string }
  | MissingAnalysis of { module_name: string; path: Path.t }
  | StoreFailure of { module_name: string; reason: string }
  | PackageStoreFailure of { package_name: string; reason: string }

let check_source_with_config = Batch.check_source_with_config

let check_source = Batch.check_source

type module_id = int

type graph_source = {
  prepared: prepared_source;
  required_local_ids: module_id array;
  missing_external_names: string list;
}

type graph_group = {
  id: module_id;
  module_name: string;
  internal_name: Session.LocalModules.InternalName.t;
  sources: graph_source list;
  local_alias_names: string list;
  public_module_names: string list;
  dependency_ids: module_id array;
}

type local_module_surface = {
  ambient_exports: TypConfig.env;
  ambient_type_decls: FileSummary.type_decl list;
}

type external_ambient = {
  ambient_exports: TypConfig.env;
  ambient_type_decls: FileSummary.type_decl list;
}

type package_graph = {
  groups: graph_group array;
  candidate_ids_by_required_name: (string, module_id list) Collections.HashMap.t;
  external_ambient: external_ambient;
}

type engine_state = {
  local_canonical_typings: LoadedModules.t;
  public_module_typings: LoadedModules.t;
  local_typings_by_id: ModuleTypings.t option array;
  local_surfaces_by_id: local_module_surface option array;
}

let dedupe_preserving_order = fun names ->
  let seen = Collections.HashSet.with_capacity (List.length names + 1) in
  names |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let dedupe_module_ids_preserving_order = fun module_ids ->
  let seen = Collections.HashSet.with_capacity (List.length module_ids + 1) in
  module_ids |> List.filter
    (fun module_id ->
      if Collections.HashSet.contains seen module_id then
        false
      else
        let _ = Collections.HashSet.insert seen module_id in
        true)

let grouped_sources_by_internal_module = fun ordered_sources ->
  let module_order_rev = ref [] in
  let sources_by_module_name = Collections.HashMap.with_capacity 64 in
  let () =
    ordered_sources
    |> List.iter
      (fun (source: prepared_source) ->
        let existing_rev =
          match Collections.HashMap.get sources_by_module_name source.internal_module_name with
          | Some existing_rev -> existing_rev
          | None ->
              module_order_rev := source.internal_module_name :: !module_order_rev;
              []
        in
        let _ = Collections.HashMap.insert
          sources_by_module_name
          source.internal_module_name
          (source :: existing_rev) in
        ())
  in
  !module_order_rev
  |> List.rev
  |> List.filter_map
    (fun module_name ->
      Collections.HashMap.get sources_by_module_name module_name
      |> Option.map (fun sources_rev -> (module_name, List.rev sources_rev)))

let is_alias_module_name = fun module_name -> String.ends_with ~suffix:"__Aliases" module_name

let module_dependencies_of_source = fun (source: prepared_source) ->
  let explicit_dependencies =
    match Syn.Deps.of_parse_result source.source.parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let current_segments = source.internal_module_name |> Session.LocalModules.split_internal_module_name in
  let should_include_implicit_open module_name =
    if List.length current_segments <= 1 || not (is_alias_module_name module_name) then
      true
    else
      let alias_segments = module_name |> Session.LocalModules.split_internal_module_name in
      match List.rev alias_segments with
      | "Aliases" :: reversed_prefix ->
          let prefix = List.rev reversed_prefix in
          not
            (List.length prefix > 0
            && List.length prefix <= List.length current_segments
            && List.for_all2 String.equal prefix (List.take (List.length prefix) current_segments))
      | _ -> true
  in
  let implicit_opens = source.source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter should_include_implicit_open in
  dedupe_preserving_order (explicit_dependencies @ implicit_opens)

let source_aliases = fun (source: prepared_source) ->
  let derived_aliases = source.internal_module_name
  |> Session.LocalModules.InternalName.of_string
  |> Session.LocalModules.local_module_aliases_of_internal_name
  |> List.map Session.LocalModules.AmbientName.to_string in
  dedupe_preserving_order
    (
      derived_aliases @ [ source.local_module_name ] @ (
        match source.public_module_name with
        | Some module_name -> [ module_name ]
        | None -> []
      )
    )

let local_alias_names_for_group = fun module_name sources ->
  let public_module_names = sources
  |> List.filter_map (fun (source: prepared_source) -> source.public_module_name)
  |> dedupe_preserving_order in
  let public_name_set = Collections.HashSet.of_list public_module_names in
  let alias_names = sources
  |> List.concat_map source_aliases
  |> List.filter (fun alias_name -> not (String.equal alias_name module_name))
  |> dedupe_preserving_order
  |> List.filter (fun alias_name -> not (Collections.HashSet.contains public_name_set alias_name)) in
  (alias_names, public_module_names)

let visible_names_of_group = fun (group: graph_group) ->
  dedupe_preserving_order
    (group.module_name
    :: (group.internal_name
    |> Session.LocalModules.local_module_aliases_of_internal_name
    |> List.map Session.LocalModules.AmbientName.to_string))

let candidate_ids_by_required_name = fun groups ->
  let by_name_rev = Collections.HashMap.with_capacity (Array.length groups * 4) in
  groups |> Array.iter
    (fun (group: graph_group) ->
      visible_names_of_group group |> List.iter
        (fun visible_name ->
          let existing_rev =
            match Collections.HashMap.get by_name_rev visible_name with
            | Some existing_rev -> existing_rev
            | None -> []
          in
          let _ = Collections.HashMap.insert by_name_rev visible_name (group.id :: existing_rev) in
          ()));
  let by_name = Collections.HashMap.with_capacity (Array.length groups * 4) in
  Collections.HashMap.iter
    (fun visible_name module_ids_rev ->
      let _ = Collections.HashMap.insert by_name visible_name (List.rev module_ids_rev) in
      ())
    by_name_rev;
  by_name

let best_matching_local_module_ids = fun graph ~current_module_name ~required_module_name ->
  let best_depth = ref None in
  let matches_rev = ref [] in
  let candidate_ids =
    match Collections.HashMap.get
      graph.candidate_ids_by_required_name
      (Session.LocalModules.RequiredName.to_string required_module_name) with
    | Some candidate_ids -> candidate_ids
    | None -> []
  in
  let () =
    candidate_ids
    |> List.iter
      (fun candidate_id ->
        let group = graph.groups.(candidate_id) in
        if
          not
            (String.equal
              group.module_name
              (Session.LocalModules.InternalName.to_string current_module_name))
        then
          match Session.LocalModules.contextual_match_depth
            ~current_module_name
            ~required_module_name
            ~candidate_module_name:group.internal_name with
          | None -> ()
          | Some depth ->
              let current_best = Option.unwrap_or ~default:depth !best_depth in
              if Option.is_none !best_depth || depth > current_best then
                (
                  best_depth := Some depth;
                  matches_rev := [ group.id ]
                )
              else if Int.equal depth current_best then
                matches_rev := group.id :: !matches_rev)
  in
  List.rev !matches_rev

let missing_requirements_of_source = fun graph config (source: prepared_source) ->
  let current_module_name = Session.LocalModules.InternalName.of_string source.internal_module_name in
  let missing_rev = ref [] in
  let required_local_ids_rev = ref [] in
  let () =
    module_dependencies_of_source source
    |> List.iter
      (fun required_module_name ->
        let required_module_name = Session.LocalModules.RequiredName.of_string required_module_name in
        let local_ids = best_matching_local_module_ids graph ~current_module_name ~required_module_name in
        if not (List.is_empty local_ids) then
          required_local_ids_rev := List.rev_append local_ids !required_local_ids_rev
        else if
          not
            (LoadedModules.contains
              TypConfig.(config.loaded_modules)
              ~module_name:(Session.LocalModules.RequiredName.to_string required_module_name))
        then
          missing_rev := Session.MissingRequirements.MissingModuleSummary {
            module_name = Session.LocalModules.RequiredName.to_string required_module_name;
            requested_by = [ source.source.source_id ]
          }
          :: !missing_rev)
  in
  (
    !required_local_ids_rev |> List.rev |> dedupe_module_ids_preserving_order |> Array.of_list,
    !missing_rev |> List.rev |> Session.MissingRequirements.of_list
  )

let external_ambient_of_loaded_modules = fun (loaded_modules: LoadedModules.t) : external_ambient ->
  let exports_rev = ref [] in
  let type_decls_rev = ref [] in
  let () =
    LoadedModules.iter
      (fun module_name typings ->
        let type_decls = ModuleTypings.type_decls typings in
        let qualified_exports = Session.ModuleSurface.qualify_exports
          ~module_name
          ~type_decls
          (ModuleTypings.exports typings) in
        let qualified_type_decls = Session.ModuleSurface.qualify_type_decls ~module_name type_decls in
        exports_rev := List.rev_append qualified_exports !exports_rev;
        type_decls_rev := List.rev_append qualified_type_decls !type_decls_rev)
      loaded_modules
  in
  { ambient_exports = List.rev !exports_rev; ambient_type_decls = List.rev !type_decls_rev }

let build_package_graph = fun ~config ~ordered_sources ->
  let grouped_sources = grouped_sources_by_internal_module ordered_sources in
  let grouped_sources_array = grouped_sources |> Array.of_list in
  let external_ambient = external_ambient_of_loaded_modules TypConfig.(config.loaded_modules) in
  let groups =
    grouped_sources_array
    |> Array.mapi
      (fun module_id (module_name, sources) ->
        let local_alias_names, public_module_names = local_alias_names_for_group module_name sources in
        {
          id = module_id;
          module_name;
          internal_name = Session.LocalModules.InternalName.of_string module_name;
          sources = [];
          local_alias_names;
          public_module_names;
          dependency_ids = [||];
        })
  in
  let graph = {
    groups;
    candidate_ids_by_required_name = candidate_ids_by_required_name groups;
    external_ambient
  } in
  let groups =
    Array.mapi
      (fun module_id (group: graph_group) ->
        let (_module_name, sources) = grouped_sources_array.(module_id) in
        let graph_sources =
          sources
          |> List.map
            (fun (source: prepared_source) ->
              let required_local_ids, missing_requirements = missing_requirements_of_source
                graph
                config
                source in
              let missing_external_names =
                Session.MissingRequirements.requirements missing_requirements
                |> List.filter_map
                  (
                    function
                    | Session.MissingRequirements.MissingModuleSummary { module_name; _ } -> Some module_name
                    | Session.MissingRequirements.MissingRootSource _
                    | Session.MissingRequirements.LocalModuleCycle _ -> None
                  )
              in
              { prepared = source; required_local_ids; missing_external_names })
        in
        let dependency_ids = graph_sources
        |> List.concat_map (fun (source: graph_source) -> source.required_local_ids |> Array.to_list)
        |> List.filter (fun dependency_id -> not (Int.equal dependency_id module_id))
        |> dedupe_module_ids_preserving_order
        |> Array.of_list in
        { group with sources = graph_sources; dependency_ids })
      groups
  in
  { graph with groups }

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
  let module_names = module_ids |> List.map (fun module_id -> graph.groups.(module_id).module_name) in
  let source_ids = module_ids
  |> List.concat_map
    (fun module_id ->
      graph.groups.(module_id).sources
      |> List.map (fun (source: graph_source) -> source.prepared.source.source_id)) in
  let module_name =
    match module_names with
    | module_name :: _ -> module_name
    | [] -> "<cycle>"
  in
  let requirements = Session.MissingRequirements.of_list
    [ Session.MissingRequirements.LocalModuleCycle { module_names; source_ids } ] in
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

let merge_loaded_modules = fun loaded_modules new_typings ->
  LoadedModules.merge
    ~preferred:(LoadedModules.of_list new_typings)
    ~fallback:loaded_modules
    ~combine:(fun existing _incoming -> existing)

let merge_public_module_typings = fun loaded_modules new_typings ->
  LoadedModules.merge
    ~preferred:(LoadedModules.of_list new_typings)
    ~fallback:loaded_modules
    ~combine:(fun existing _incoming -> existing)

let persist_module_typings = fun store module_typings ->
  match Store.save_module_typings store module_typings with
  | Ok () -> Ok ()
  | Error reason -> Error reason

let persist_module_views = fun config ~module_name typings ->
  match config.TypConfig.store with
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
  match (config.TypConfig.store, package_name, package_fingerprint, typings) with
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

let rebind_module_views = fun (group: graph_group) module_typings ->
  let local_alias_typings = group.local_alias_names
  |> List.map
    (fun alias_name -> Session.ModuleSurface.rebind_module_typings ~module_name:alias_name module_typings) in
  let public_module_typings = group.public_module_names
  |> List.map
    (fun public_name -> Session.ModuleSurface.rebind_module_typings ~module_name:public_name module_typings) in
  (local_alias_typings, public_module_typings)

let local_surface_of_typings = fun module_name module_typings : local_module_surface ->
  let visible_names = module_name
  |> Session.LocalModules.InternalName.of_string
  |> Session.LocalModules.ambient_names_of_internal_name
  |> List.map Session.LocalModules.AmbientName.to_string in
  let type_decls = ModuleTypings.type_decls module_typings in
  let exports = ModuleTypings.exports module_typings in
  let ambient_exports = visible_names
  |> List.concat_map
    (fun visible_name ->
      Session.ModuleSurface.qualify_exports ~module_name:visible_name ~type_decls exports) in
  let ambient_type_decls = visible_names
  |> List.concat_map
    (fun visible_name -> Session.ModuleSurface.qualify_type_decls ~module_name:visible_name type_decls) in
  { ambient_exports; ambient_type_decls }

let source_config = fun ~graph ~state ~base_config (source: graph_source) ->
  let module_name = source.prepared.internal_module_name in
  let unavailable_local_ids = source.required_local_ids
  |> Array.to_list
  |> List.filter (fun module_id -> Option.is_none state.local_typings_by_id.(module_id)) in
  let missing_requirements = (source.missing_external_names
  |> List.map
    (fun missing_module_name ->
      Session.MissingRequirements.MissingModuleSummary {
        module_name = missing_module_name;
        requested_by = [ source.prepared.source.source_id ]
      }))
  @ (unavailable_local_ids
  |> List.map
    (fun module_id ->
      Session.MissingRequirements.MissingModuleSummary {
        module_name = graph.groups.(module_id).module_name;
        requested_by = [ source.prepared.source.source_id ]
      })) in
  match missing_requirements with
  | _ :: _ -> Error (MissingRequirements {
    module_name;
    requirements = Session.MissingRequirements.of_list missing_requirements
  })
  | [] ->
      let local_exports_rev = ref [] in
      let local_type_decls_rev = ref [] in
      let () =
        source.required_local_ids
        |> Array.iter
          (fun module_id ->
            match state.local_surfaces_by_id.(module_id) with
            | Some surface ->
                local_exports_rev := List.rev_append surface.ambient_exports !local_exports_rev;
                local_type_decls_rev := List.rev_append surface.ambient_type_decls !local_type_decls_rev
            | None -> ())
      in
      let loaded_modules =
        LoadedModules.merge
          ~preferred:state.local_canonical_typings
          ~fallback:base_config.TypConfig.loaded_modules
          ~combine:(fun existing _incoming -> existing)
      in
      let ambient = base_config.ambient @ graph.external_ambient.ambient_exports @ List.rev !local_exports_rev in
      let ambient_type_decls = base_config.ambient_type_decls
      @ graph.external_ambient.ambient_type_decls
      @ List.rev !local_type_decls_rev in
      Ok (base_config
      |> TypConfig.with_loaded_module_index ~loaded_modules
      |> TypConfig.with_ambient ~ambient
      |> TypConfig.with_ambient_type_decls ~ambient_type_decls)

let analyze_group = fun ~graph ~state ~config (group: graph_group) ->
  let analyzed_sources =
    group.sources
    |> List.fold_left
      (fun result (source: graph_source) ->
        match result with
        | Error _ as err -> err
        | Ok analyzed_sources -> (
            match source_config ~graph ~state ~base_config:config source with
            | Error _ as err -> err
            | Ok source_config ->
                let analysis = Session.SourceAnalysis.analyze ~config:source_config source.prepared.source in
                Ok ((source.prepared, analysis) :: analyzed_sources)
          ))
      (Ok [])
  in
  match analyzed_sources with
  | Error _ as err -> err
  | Ok analyzed_sources -> Ok (List.rev analyzed_sources)

let fold_package_sources = fun ?package_name ?package_fingerprint ~config ~ordered_sources ~init ~f () ->
  let graph = build_package_graph ~config ~ordered_sources in
  match ordered_group_ids graph with
  | Error _ as err -> err
  | Ok ordered_group_ids ->
      let initial_state = {
        local_canonical_typings = LoadedModules.empty;
        public_module_typings = LoadedModules.empty;
        local_typings_by_id = Array.make (Array.length graph.groups) None;
        local_surfaces_by_id = Array.make (Array.length graph.groups) None
      } in
      let result =
        ordered_group_ids
        |> List.fold_left
          (fun result module_id ->
            match result with
            | Error _ as err -> err
            | Ok (acc, state) ->
                let group = graph.groups.(module_id) in
                (
                  match analyze_group ~graph ~state ~config group with
                  | Error _ as err -> err
                  | Ok analyzed_sources ->
                      let pairing = analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis) -> (source.source, analysis))
                      |> Session.ModulePairing.of_sources ~module_name:group.module_name in
                      let checked_sources = analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis) ->
                          { path = source.display_path; analysis }) in
                      let module_typings = pairing.module_typings in
                      let local_alias_typings, public_module_typings = rebind_module_views group module_typings in
                      let persisted_typings = module_typings :: public_module_typings in
                      (
                        match persist_module_views config ~module_name:group.module_name persisted_typings with
                        | Error _ as err -> err
                        | Ok () ->
                            let local_canonical_typings = merge_loaded_modules
                              state.local_canonical_typings
                              [ module_typings ] in
                            let public_module_typings_index = merge_public_module_typings
                              state.public_module_typings
                              public_module_typings in
                            let local_typings_by_id = Array.copy state.local_typings_by_id in
                            local_typings_by_id.(module_id) <- Some module_typings;
                            let local_surfaces_by_id = Array.copy state.local_surfaces_by_id in
                            local_surfaces_by_id.(module_id) <- Some (local_surface_of_typings
                              group.module_name
                              module_typings);
                            let loaded_modules =
                              LoadedModules.merge
                                ~preferred:local_canonical_typings
                                ~fallback:config.TypConfig.loaded_modules
                                ~combine:(fun existing _incoming -> existing)
                            in
                            let finished_group = {
                              module_name = group.module_name;
                              checked_sources;
                              module_typings;
                              loaded_modules
                            } in
                            let state = {
                              local_canonical_typings;
                              public_module_typings = public_module_typings_index;
                              local_typings_by_id;
                              local_surfaces_by_id
                            } in
                            Ok (f acc finished_group, state)
                      )
                ))
          (Ok (init, initial_state))
      in
      match result with
      | Error _ as err -> err
      | Ok (acc, state) -> (
          match persist_package_bundle config ?package_name ?package_fingerprint state.public_module_typings with
          | Error _ as err -> err
          | Ok () ->
              let loaded_modules =
                LoadedModules.merge
                  ~preferred:state.local_canonical_typings
                  ~fallback:config.TypConfig.loaded_modules
                  ~combine:(fun existing _incoming -> existing)
              in
              Ok { acc; loaded_modules; public_module_typings = state.public_module_typings }
        )
