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

type source_identity = {
  internal_name: Session.LocalModules.InternalName.t;
  local_name: Session.LocalModules.AmbientName.t;
  public_name: Session.LocalModules.AmbientName.t option;
}

type visible_module_name =
  | InternalName of Session.LocalModules.InternalName.t
  | AmbientName of Session.LocalModules.AmbientName.t

type resolved_source = {
  prepared: prepared_source;
  identity: source_identity;
}

type graph_source = {
  prepared: resolved_source;
  required_local_ids: module_id array;
  missing_external_names: Session.LocalModules.RequiredName.t array;
}

type graph_group = {
  id: module_id;
  internal_name: Session.LocalModules.InternalName.t;
  visible_names: visible_module_name array;
  sources: graph_source list;
  local_alias_names: Session.LocalModules.AmbientName.t array;
  public_module_names: Session.LocalModules.AmbientName.t array;
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
  candidate_ids_by_required_name:
    (Session.LocalModules.RequiredName.t, module_id array) Collections.HashMap.t;
  external_ambient: external_ambient;
}

type engine_state = {
  loaded_modules: LoadedModules.t;
  public_module_typings: LoadedModules.t;
  local_typings_by_id: ModuleTypings.t option array;
  local_surfaces_by_id: local_module_surface option array;
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

let source_identity_of_prepared_source = fun (source: prepared_source) ->
  {
    internal_name = Session.LocalModules.InternalName.of_string source.internal_module_name;
    local_name = Session.LocalModules.AmbientName.of_string source.local_module_name;
    public_name = source.public_module_name |> Option.map Session.LocalModules.AmbientName.of_string
  }

let module_name_of_internal_name = Session.LocalModules.InternalName.to_string

let module_name_of_source_identity = fun (identity: source_identity) ->
  module_name_of_internal_name identity.internal_name

let visible_module_name_to_string = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> module_name_of_internal_name internal_name
  | AmbientName ambient_name -> Session.LocalModules.AmbientName.to_string ambient_name

let required_name_of_visible_module_name = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> Session.LocalModules.RequiredName.of_internal_name internal_name
  | AmbientName ambient_name -> Session.LocalModules.RequiredName.of_ambient_name ambient_name

let ambient_names_of_source_identity = fun (identity: source_identity) ->
  dedupe_by_key_preserving_order ~key:Session.LocalModules.AmbientName.to_string
    (
      Session.LocalModules.local_module_aliases_of_internal_name identity.internal_name
      @ [ identity.local_name ]
      @ (
        match identity.public_name with
        | Some public_name -> [ public_name ]
        | None -> []
      )
    )

let visible_names_of_source_identity = fun (identity: source_identity) ->
  dedupe_by_key_preserving_order
    ~key:visible_module_name_to_string
    (InternalName identity.internal_name
    :: (ambient_names_of_source_identity identity
    |> List.map (fun ambient_name -> AmbientName ambient_name)))

let grouped_sources_by_internal_module = fun ordered_sources ->
  let module_order_rev = ref [] in
  let sources_by_module_name = Collections.HashMap.with_capacity 64 in
  ordered_sources |> List.iter
    (fun (source: prepared_source) ->
      let resolved_source = {
        prepared = source;
        identity = source_identity_of_prepared_source source
      } in
      let module_name = module_name_of_source_identity resolved_source.identity in
      let existing_rev =
        match Collections.HashMap.get sources_by_module_name module_name with
        | Some existing_rev -> existing_rev
        | None ->
            module_order_rev := module_name :: !module_order_rev;
            []
      in
      let _ = Collections.HashMap.insert
        sources_by_module_name
        module_name
        (resolved_source :: existing_rev) in
      ());
  !module_order_rev
  |> List.rev
  |> List.filter_map
    (fun module_name ->
      Collections.HashMap.get sources_by_module_name module_name
      |> Option.map (fun sources_rev -> (module_name, List.rev sources_rev)))

let is_alias_module_name = fun module_name -> String.ends_with ~suffix:"__Aliases" module_name

let required_names_of_source = fun (source: resolved_source) ->
  let explicit_dependencies =
    match Syn.Deps.of_parse_result source.prepared.source.parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let current_segments = module_name_of_source_identity source.identity |> Session.LocalModules.split_internal_module_name in
  let should_include_implicit_open module_name =
    if List.length current_segments <= 1 || not (is_alias_module_name module_name) then
      true
    else
      let alias_segments = module_name |> Session.LocalModules.split_internal_module_name in
      match List.rev alias_segments with
      | "Aliases" :: reversed_prefix ->
          let prefix = List.rev reversed_prefix in
          not
            (not (List.is_empty prefix)
            && List.length prefix <= List.length current_segments
            && List.for_all2 String.equal prefix (List.take (List.length prefix) current_segments))
      | _ -> true
  in
  let implicit_opens = source.prepared.source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter should_include_implicit_open in
  dedupe_by_key_preserving_order ~key:Session.LocalModules.RequiredName.to_string
    ((explicit_dependencies @ implicit_opens) |> List.map Session.LocalModules.RequiredName.of_string)

let visible_name_arrays_for_group = fun internal_name sources ->
  let visible_names = sources
  |> List.concat_map
    (fun (source: resolved_source) -> visible_names_of_source_identity source.identity)
  |> dedupe_by_key_preserving_order ~key:visible_module_name_to_string in
  let public_module_names = sources
  |> List.filter_map (fun (source: resolved_source) -> source.identity.public_name)
  |> dedupe_by_key_preserving_order ~key:Session.LocalModules.AmbientName.to_string in
  let public_name_set = public_module_names
  |> List.map Session.LocalModules.AmbientName.to_string
  |> Collections.HashSet.of_list in
  let alias_names =
    visible_names
    |> List.filter_map
      (
        function
        | InternalName _ -> None
        | AmbientName ambient_name ->
            let ambient_name_string = Session.LocalModules.AmbientName.to_string ambient_name in
            if
              String.equal ambient_name_string (module_name_of_internal_name internal_name)
              || Collections.HashSet.contains public_name_set ambient_name_string
            then
              None
            else
              Some ambient_name
      )
    |> dedupe_by_key_preserving_order ~key:Session.LocalModules.AmbientName.to_string
  in
  (Array.of_list visible_names, Array.of_list alias_names, Array.of_list public_module_names)

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
        match Session.LocalModules.contextual_match_depth
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
  |> dedupe_by_key_preserving_order ~key:Session.LocalModules.RequiredName.to_string
  |> List.iter
    (fun required_name ->
      let local_ids = best_matching_local_module_ids graph group ~required_module_name:required_name in
      let _ = Collections.HashMap.insert by_name required_name local_ids in
      ());
  by_name

let missing_requirements_of_source = fun ~config ~resolution_ids_by_required_name ~(source:resolved_source) ~required_names ->
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
          (LoadedModules.contains
            TypConfig.(config.loaded_modules)
            ~module_name:(Session.LocalModules.RequiredName.to_string required_module_name))
      then
        missing_rev := Session.MissingRequirements.MissingModuleSummary {
          module_name = Session.LocalModules.RequiredName.to_string required_module_name;
          requested_by = [ source.prepared.source.source_id ]
        }
        :: !missing_rev);
  (
    !required_local_ids_rev |> List.rev |> dedupe_module_ids_preserving_order |> Array.of_list,
    !missing_rev |> List.rev |> Session.MissingRequirements.of_list
  )

let external_ambient_of_loaded_modules = fun (loaded_modules: LoadedModules.t) : external_ambient ->
  let exports_rev = ref [] in
  let type_decls_rev = ref [] in
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
    loaded_modules;
  { ambient_exports = List.rev !exports_rev; ambient_type_decls = List.rev !type_decls_rev }

let build_package_graph = fun ~config ~ordered_sources ->
  let grouped_sources = grouped_sources_by_internal_module ordered_sources in
  let grouped_sources_array = grouped_sources |> Array.of_list in
  let external_ambient = external_ambient_of_loaded_modules TypConfig.(config.loaded_modules) in
  let groups =
    grouped_sources_array
    |> Array.mapi
      (fun module_id (module_name, sources) ->
        let internal_name = Session.LocalModules.InternalName.of_string module_name in
        let visible_names, local_alias_names, public_module_names = visible_name_arrays_for_group
          internal_name
          sources in
        {
          id = module_id;
          internal_name;
          visible_names;
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
        let source_requirements = sources
        |> List.map (fun (source: resolved_source) -> (source, required_names_of_source source)) in
        let resolution_ids_by_required_name = source_requirements
        |> List.concat_map snd
        |> resolution_ids_by_required_name graph group in
        let graph_sources =
          source_requirements
          |> List.map
            (fun ((source: resolved_source), required_names) ->
              let required_local_ids, missing_requirements = missing_requirements_of_source
                ~config
                ~resolution_ids_by_required_name
                ~source
                ~required_names in
              let missing_external_names =
                Session.MissingRequirements.requirements missing_requirements
                |> List.filter_map
                  (
                    function
                    | Session.MissingRequirements.MissingModuleSummary { module_name; _ } -> Some (Session.LocalModules.RequiredName.of_string
                      module_name)
                    | Session.MissingRequirements.MissingRootSource _
                    | Session.MissingRequirements.LocalModuleCycle _ -> None
                  )
                |> Array.of_list
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
  let module_names = module_ids
  |> List.map (fun module_id -> module_name_of_internal_name graph.groups.(module_id).internal_name) in
  let source_ids = module_ids
  |> List.concat_map
    (fun module_id ->
      graph.groups.(module_id).sources
      |> List.map (fun (source: graph_source) -> source.prepared.prepared.source.source_id)) in
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

let rebind_module_views = fun (group: graph_group) module_typings ->
  let local_alias_typings = group.local_alias_names
  |> Array.to_list
  |> List.map
    (fun alias_name ->
      Session.ModuleSurface.rebind_module_typings
        ~module_name:(Session.LocalModules.AmbientName.to_string alias_name)
        module_typings) in
  let public_module_typings = group.public_module_names
  |> Array.to_list
  |> List.map
    (fun public_name ->
      Session.ModuleSurface.rebind_module_typings
        ~module_name:(Session.LocalModules.AmbientName.to_string public_name)
        module_typings) in
  (local_alias_typings, public_module_typings)

let local_surface_of_typings = fun (group: graph_group) module_typings : local_module_surface ->
  let visible_names = group.visible_names |> Array.to_list |> List.map visible_module_name_to_string in
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
  let module_name = module_name_of_source_identity source.prepared.identity in
  let unavailable_local_ids = source.required_local_ids
  |> Array.to_list
  |> List.filter (fun module_id -> Option.is_none state.local_typings_by_id.(module_id)) in
  let missing_requirements = (source.missing_external_names
  |> Array.to_list
  |> List.map
    (fun missing_module_name ->
      Session.MissingRequirements.MissingModuleSummary {
        module_name = Session.LocalModules.RequiredName.to_string missing_module_name;
        requested_by = [ source.prepared.prepared.source.source_id ]
      }))
  @ (unavailable_local_ids
  |> List.map
    (fun module_id ->
      Session.MissingRequirements.MissingModuleSummary {
        module_name = module_name_of_internal_name graph.groups.(module_id).internal_name;
        requested_by = [ source.prepared.prepared.source.source_id ]
      })) in
  match missing_requirements with
  | _ :: _ -> Error (MissingRequirements {
    module_name;
    requirements = Session.MissingRequirements.of_list missing_requirements
  })
  | [] ->
      let local_exports_rev = ref [] in
      let local_type_decls_rev = ref [] in
      source.required_local_ids |> Array.iter
        (fun module_id ->
          match state.local_surfaces_by_id.(module_id) with
          | Some surface ->
              local_exports_rev := List.rev_append surface.ambient_exports !local_exports_rev;
              local_type_decls_rev := List.rev_append surface.ambient_type_decls !local_type_decls_rev
          | None -> ());
      let ambient = TypConfig.(base_config.ambient)
      @ graph.external_ambient.ambient_exports
      @ List.rev !local_exports_rev in
      let ambient_type_decls = TypConfig.(base_config.ambient_type_decls)
      @ graph.external_ambient.ambient_type_decls
      @ List.rev !local_type_decls_rev in
      Ok (base_config
      |> TypConfig.with_loaded_module_index ~loaded_modules:state.loaded_modules
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
                let analysis = Session.SourceAnalysis.analyze
                  ~config:source_config
                  source.prepared.prepared.source in
                Ok ((source.prepared.prepared, analysis) :: analyzed_sources)
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
        loaded_modules = LoadedModules.copy TypConfig.(config.loaded_modules);
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
                      let module_name = module_name_of_internal_name group.internal_name in
                      let pairing = analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis) -> (source.source, analysis))
                      |> Session.ModulePairing.of_sources ~module_name in
                      let checked_sources = analyzed_sources
                      |> List.map
                        (fun ((source: prepared_source), analysis) ->
                          { path = source.display_path; analysis }) in
                      let module_typings = pairing.module_typings in
                      let local_alias_typings, public_module_typings = rebind_module_views group module_typings in
                      let persisted_typings = module_typings :: public_module_typings in
                      (
                        match persist_module_views config ~module_name persisted_typings with
                        | Error _ as err -> err
                        | Ok () ->
                            LoadedModules.add state.loaded_modules module_typings;
                            public_module_typings
                            |> List.iter (LoadedModules.add state.public_module_typings);
                            state.local_typings_by_id.(module_id) <- Some module_typings;
                            state.local_surfaces_by_id.(module_id) <- Some (local_surface_of_typings
                              group
                              module_typings);
                            let finished_group = {
                              module_name;
                              checked_sources;
                              module_typings;
                              loaded_modules = state.loaded_modules
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
          | Ok () -> Ok {
            acc;
            loaded_modules = state.loaded_modules;
            public_module_typings = state.public_module_typings
          }
        )
