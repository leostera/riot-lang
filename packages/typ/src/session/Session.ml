open Std
open Model
module Snapshot = Snapshot
module SourceAnalysis = SourceAnalysis
module ModulePairing = ModulePairing
module ModuleSurface = ModuleSurface
module MissingRequirements = MissingRequirements
module LocalModules = LocalModules

type t = {
  config: TypConfig.t;
  next_source_id: int;
  next_revision: int;
  sources: Source.t list;
  shared_snapshot_caches: Snapshot.SharedCaches.t;
  sources_by_id: (int, Source.t) Collections.HashMap.t;
  source_ids_by_module_name: (string, SourceId.t list) Collections.HashMap.t;
  parse_results_by_source_hash: (string, Syn.Parser.parse_result) Collections.HashMap.t;
  declared_modules_by_source_hash: (string, string list) Collections.HashMap.t;
  module_dependencies_by_env_and_source_hash: (string, string list) Collections.HashMap.t;
  deps_envs_by_loaded_modules: (string, Syn.Deps.Env.t) Collections.HashMap.t;
}

let empty = fun ~config ->
  {
    config;
    next_source_id = 0;
    next_revision = 0;
    sources = [];
    shared_snapshot_caches = Snapshot.SharedCaches.create ();
    sources_by_id = Collections.HashMap.with_capacity 64;
    source_ids_by_module_name = Collections.HashMap.with_capacity 32;
    parse_results_by_source_hash = Collections.HashMap.with_capacity 64;
    declared_modules_by_source_hash = Collections.HashMap.with_capacity 64;
    module_dependencies_by_env_and_source_hash = Collections.HashMap.with_capacity 256;
    deps_envs_by_loaded_modules = Collections.HashMap.with_capacity 16;
  }

let config = fun session -> session.config

let with_config = fun session ~config -> { session with config }

let source_hash_key = fun (source: Source.t) -> Source.input_hash source |> Crypto.Digest.hex

let filename_of_source = fun (source: Source.t) ->
  match source.origin with
  | Source.Path path -> path
  | Source.Label label -> Path.of_string label |> Result.unwrap_or ~default:(Path.v "<fragment>")

let source_of_id = fun session source_id ->
  Collections.HashMap.get session.sources_by_id (SourceId.to_int source_id)

let source_ids_of_module = fun session module_name ->
  match Collections.HashMap.get session.source_ids_by_module_name module_name with
  | Some source_ids -> source_ids
  | None -> []

let module_name_index_keys = fun (source: Source.t) ->
  let internal_name = LocalModules.InternalName.of_string (Source.module_name source) in
  (Source.module_name source)
  :: (LocalModules.local_module_aliases_of_internal_name internal_name
  |> List.map LocalModules.AmbientName.to_string)
  |> List.sort_uniq String.compare

let local_source_ids_for_module = fun session module_name -> source_ids_of_module session module_name

let local_source_ids_for_module_in_scope = fun session ~current_module_name module_name ->
  let current_module_name = LocalModules.InternalName.of_string current_module_name in
  let required_module_name = LocalModules.RequiredName.of_string module_name in
  let matching_sources = source_ids_of_module session module_name
  |> List.filter_map (source_of_id session)
  |> List.filter_map
    (fun (source: Source.t) ->
      LocalModules.contextual_match_depth
        ~current_module_name
        ~required_module_name
        ~candidate_module_name:(LocalModules.InternalName.of_string (Source.module_name source))
      |> Option.map (fun depth -> (source.source_id, depth))) in
  let best_depth = matching_sources
  |> List.fold_left
    (fun best (_, depth) -> Some (Option.unwrap_or ~default:depth best |> Int.max depth))
    None in
  match best_depth with
  | None -> []
  | Some best_depth ->
      matching_sources |> List.filter_map
        (fun (source_id, depth) ->
          if Int.equal depth best_depth then
            Some source_id
          else
            None) |> List.sort_uniq SourceId.compare

let has_local_source_for_module = fun session module_name ->
  match local_source_ids_for_module session module_name with
  | _ :: _ -> true
  | [] -> false

let root_module_names = fun session roots ->
  roots
  |> List.filter_map (source_of_id session)
  |> List.map Source.module_name
  |> List.sort_uniq String.compare

let replace_source = fun sources updated ->
  sources |> List.map
    (fun (source: Source.t) ->
      if SourceId.equal source.source_id Source.(updated.source_id) then
        updated
      else
        source)

let add_module_name_index = fun session ~module_name ~source_id ->
  let source_ids =
    match Collections.HashMap.get session.source_ids_by_module_name module_name with
    | Some source_ids when List.exists (SourceId.equal source_id) source_ids -> source_ids
    | Some source_ids -> source_ids @ [ source_id ]
    | None -> [ source_id ]
  in
  let _ = Collections.HashMap.insert session.source_ids_by_module_name module_name source_ids in
  session

let add_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source in
  module_name_index_keys source
  |> List.fold_left
    (fun session module_name -> add_module_name_index session ~module_name ~source_id:source.source_id)
    session

let update_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source in
  session

let remove_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.remove session.sources_by_id (SourceId.to_int source.source_id) in
  let module_names = Collections.HashMap.keys session.source_ids_by_module_name in
  module_names |> List.iter
    (fun module_name ->
      let remaining_source_ids = source_ids_of_module session module_name
      |> List.filter
        (fun current_source_id -> not (SourceId.equal current_source_id source.source_id)) in
      if List.is_empty remaining_source_ids then
        let _ = Collections.HashMap.remove session.source_ids_by_module_name module_name in
        ()
      else
        let _ = Collections.HashMap.insert session.source_ids_by_module_name module_name remaining_source_ids in
        ());
  session

let loaded_modules_key = fun loaded_modules ->
  LoadedModules.stable_key loaded_modules

let create_source = fun session ~kind ~module_name ~implicit_opens ~origin ~source_hash ~parse_result ~cst ->
  let source_id = SourceId.of_int session.next_source_id in
  let source = Source.make_prepared
    ~source_id
    ~kind
    ~module_name
    ~implicit_opens
    ~origin
    ~revision:session.next_revision
    ~source_hash
    ~parse_result
    ~cst in
  let session = {
    session
    with next_source_id = session.next_source_id + 1;
    next_revision = session.next_revision + 1;
    sources = session.sources @ [ source ]
  }
  |> fun session -> add_source_indexes session source in
  (session, source_id)

let register_source_alias = fun session source_id ~module_name ->
  match source_of_id session source_id with
  | None -> session
  | Some _ -> add_module_name_index session ~module_name ~source_id

let merge_loaded_modules = fun preferred fallback ->
  LoadedModules.merge
    ~preferred
    ~fallback:(LoadedModules.of_list fallback)
    ~combine:(fun existing _incoming -> existing)

let update_source = fun session source_id ~source_hash ~parse_result ~cst ->
  let revision = session.next_revision in
  match source_of_id session source_id with
  | None -> { session with next_revision = revision + 1 }
  | Some source ->
      let updated = Source.make_prepared
        ~source_id:source.source_id
        ~kind:source.kind
        ~module_name:source.module_name
        ~implicit_opens:source.implicit_opens
        ~origin:source.origin
        ~revision
        ~source_hash
        ~parse_result
        ~cst in
      { session with next_revision = revision + 1; sources = replace_source session.sources updated }
      |> fun session -> update_source_indexes session updated

let remove_source = fun session source_id ->
  match source_of_id session source_id with
  | None -> { session with next_revision = session.next_revision + 1 }
  | Some source -> {
    session
    with next_revision = session.next_revision + 1;
    sources = List.filter
      (fun (current_source: Source.t) -> not (SourceId.equal current_source.source_id source_id))
      session.sources
  }
  |> fun session -> remove_source_indexes session source

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let module_segments_of_export_name = fun export_name ->
  let rec loop acc = function
    | segment :: rest when String.length segment > 0 && is_uppercase_ascii segment.[0] -> loop
      (segment :: acc)
      rest
    | _ -> List.rev acc
  in
  SurfacePath.to_segments export_name |> loop []

let nested_module_prefixes_of_typings = fun module_typings ->
  let export_prefixes =
    ModuleTypings.exports module_typings
    |> List.map fst
    |> List.filter_map
      (fun export_name ->
        match module_segments_of_export_name export_name with
        | head :: _ -> Some head
        | [] -> None)
  in
  let type_decl_prefixes =
    ModuleTypings.type_decls module_typings
    |> List.filter_map
      (fun (type_decl: FileSummary.type_decl) ->
        match SurfacePath.to_segments type_decl.scope_path with
        | head :: _ when String.length head > 0 && is_uppercase_ascii head.[0] -> Some head
        | _ -> None)
  in
  export_prefixes @ type_decl_prefixes |> List.sort_uniq String.compare

let deps_env_for_loaded_modules = fun session loaded_modules ->
  let key = loaded_modules_key loaded_modules in
  match Collections.HashMap.get session.deps_envs_by_loaded_modules key with
  | Some env -> (key, env)
  | None ->
      let add_summary_paths env summary =
        let module_name = ModuleTypings.module_name summary in
        let env = Syn.Deps.Env.add_path env ~path:[ module_name ] ~free_names:[ module_name ] in
        let env =
          ModuleTypings.exports summary
          |> List.fold_left
            (fun env (export_name, _) ->
              match module_segments_of_export_name export_name with
              | [] -> env
              | segments -> Syn.Deps.Env.add_path
                env
                ~path:(module_name :: segments)
                ~free_names:[ module_name ])
            env
        in
        ModuleTypings.type_decls summary |> List.fold_left
          (fun env (type_decl: FileSummary.type_decl) ->
            if SurfacePath.is_empty type_decl.scope_path then
              env
            else
              Syn.Deps.Env.add_path
                env
                ~path:(module_name :: SurfacePath.to_segments type_decl.scope_path)
                ~free_names:[ module_name ])
          env
      in
      let env = LoadedModules.fold
        (fun _module_name summary env -> add_summary_paths env summary)
        loaded_modules
        Syn.Deps.Env.empty in
      let _ = Collections.HashMap.insert session.deps_envs_by_loaded_modules key env in
      (key, env)

let parse_result_for_source = fun session (source: Source.t) ->
  let _ = session in
  source.parse_result

let declared_modules = fun session (source: Source.t) ->
  let key = source_hash_key source in
  match Collections.HashMap.get session.declared_modules_by_source_hash key with
  | Some modules -> modules
  | None ->
      let modules =
        match source.cst with
        | Syn.Cst.Implementation implementation ->
            Syn.Cst.(implementation.items) |> List.filter_map
              (fun (item: Syn.Cst.StructureItem.t) ->
                match item with
                | Syn.Cst.StructureItem.ModuleDeclaration declaration -> Some (Syn.Cst.ModuleStructure.name
                  declaration)
                | _ -> None)
        | Syn.Cst.Interface interface ->
            Syn.Cst.(interface.items) |> List.filter_map
              (fun (item: Syn.Cst.SignatureItem.t) ->
                match item with
                | Syn.Cst.SignatureItem.ModuleDeclaration declaration -> Some (Syn.Cst.ModuleSignature.name
                  declaration)
                | _ -> None)
      in
      let _ = Collections.HashMap.insert session.declared_modules_by_source_hash key modules in
      modules

let has_top_level_include_statement = fun (source: Source.t) ->
  match source.cst with
  | Syn.Cst.Implementation implementation ->
      Syn.Cst.(implementation.items) |> List.exists
        (
          function
          | Syn.Cst.StructureItem.IncludeStatement _ -> true
          | _ -> false
        )
  | Syn.Cst.Interface interface ->
      Syn.Cst.(interface.items) |> List.exists
        (
          function
          | Syn.Cst.SignatureItem.IncludeStatement _ -> true
          | _ -> false
        )

let module_dependencies = fun session ~deps_env_key ~deps_env (source: Source.t) ->
  let key = format Format.[ str deps_env_key; str ":"; str (source_hash_key source) ] in
  match Collections.HashMap.get session.module_dependencies_by_env_and_source_hash key with
  | Some modules -> modules
  | None ->
      let modules =
        match Syn.Deps.of_parse_result ~env:deps_env (parse_result_for_source session source) with
        | Ok deps -> Syn.Deps.modules deps
        | Error _ -> []
      in
      let _ = Collections.HashMap.insert session.module_dependencies_by_env_and_source_hash key modules in
      modules

let collect_missing_module_summaries = fun session roots ->
  let (deps_env_key, deps_env) = deps_env_for_loaded_modules session session.config.loaded_modules in
  let loaded_module_names = session.config.loaded_modules
  |> LoadedModules.names
  |> Collections.HashSet.of_list in
  let implicit_open_module_names (source: Source.t) = source.implicit_opens |> List.map SurfacePath.to_string in
  let implicit_open_source_ids source = implicit_open_module_names source
  |> List.concat_map
    (fun module_name ->
      local_source_ids_for_module_in_scope session ~current_module_name:(Source.module_name source) module_name) in
  let local_dependency_source_ids source module_name = local_source_ids_for_module_in_scope
    session
    ~current_module_name:(Source.module_name source)
    module_name in
  let dependency_is_loaded_only module_name =
    Collections.HashSet.contains loaded_module_names module_name
    && not (has_local_source_for_module session module_name) in
  let initial_source_ids =
    roots
    |> List.filter_map
      (fun source_id ->
        source_of_id session source_id |> Option.map
          (fun (source: Source.t) ->
            let siblings = source_ids_of_module session (Source.module_name source) in
            if List.is_empty siblings then
              [ source_id ]
            else
              siblings))
    |> List.flatten
    |> List.sort_uniq SourceId.compare
  in
  let local_nested_module_prefixes_cache = Collections.HashMap.with_capacity 32 in
  let module_source_closure initial_source_ids =
    let seen = Collections.HashSet.with_capacity (List.length initial_source_ids) in
    let rec discover to_visit seen =
      match to_visit with
      | [] -> seen
      | source_id :: rest ->
          if Collections.HashSet.contains seen (SourceId.to_int source_id) then
            discover rest seen
          else
            (
              let _ = Collections.HashSet.insert seen (SourceId.to_int source_id) in
              match source_of_id session source_id with
              | None -> discover rest seen
              | Some source ->
                  let additional = (implicit_open_source_ids source)
                  @ (module_dependencies session ~deps_env_key ~deps_env source
                  |> List.concat_map (local_dependency_source_ids source)) in
                  discover (additional @ rest) seen
            )
    in
    let closure_source_ids = discover initial_source_ids seen in
    session.sources |> List.filter
      (fun (source: Source.t) ->
        Collections.HashSet.contains closure_source_ids (SourceId.to_int source.source_id))
  in
  let local_nested_module_snapshot = ref None in
  let local_nested_module_snapshot () =
    match !local_nested_module_snapshot with
    | Some snapshot -> snapshot
    | None ->
        let sources = module_source_closure initial_source_ids in
        let snapshot = Snapshot.make_with_shared_caches
          ~revision:session.next_revision
          ~roots:initial_source_ids
          ~config:session.config
          ~sources
          ~shared_caches:session.shared_snapshot_caches in
        local_nested_module_snapshot := Some snapshot;
        snapshot
  in
  let local_nested_module_prefixes module_name =
    match Collections.HashMap.get local_nested_module_prefixes_cache module_name with
    | Some nested_modules -> nested_modules
    | None ->
        let module_sources = source_ids_of_module session module_name
        |> List.filter_map (source_of_id session) in
        let declared_nested_modules = module_sources |> List.concat_map (declared_modules session) in
        let has_include_statement = List.exists has_top_level_include_statement module_sources in
        let exported_nested_modules =
          if not has_include_statement then
            []
          else
            match Snapshot.find_module_typings_by_name (local_nested_module_snapshot ()) module_name with
            | Some module_typings -> nested_module_prefixes_of_typings module_typings
            | None -> []
        in
        let nested_modules = declared_nested_modules @ exported_nested_modules
        |> List.sort_uniq String.compare in
        let _ = Collections.HashMap.insert local_nested_module_prefixes_cache module_name nested_modules in
        nested_modules
  in
  let loaded_nested_module_prefixes module_name =
    match LoadedModules.get session.config.loaded_modules ~module_name with
    | None -> []
    | Some summary -> nested_module_prefixes_of_typings summary
  in
  let implicit_open_nested_modules source =
    implicit_open_module_names source
    |> List.concat_map
      (fun module_name ->
        let loaded_nested_modules =
          if Collections.HashSet.contains loaded_module_names module_name then
            loaded_nested_module_prefixes module_name
          else
            []
        in
        local_nested_module_prefixes module_name @ loaded_nested_modules)
    |> List.sort_uniq String.compare
  in
  let rec add_missing missing module_name requested_by =
    let rec loop = function
      | [] ->
          [ (module_name, [ requested_by ]) ]
      | (name, requesters) :: tail when String.equal name module_name ->
          let updated_requesters =
            if List.exists (SourceId.equal requested_by) requesters then
              requesters
            else
              requesters @ [ requested_by ]
          in
          (name, updated_requesters) :: tail
      | head :: tail ->
          head :: loop tail
    in
    loop missing
  in
  let seen = Collections.HashSet.with_capacity (List.length initial_source_ids) in
  let rec discover to_visit seen missing =
    match to_visit with
    | [] -> missing
    | source_id :: rest ->
        if Collections.HashSet.contains seen (SourceId.to_int source_id) then
          discover rest seen missing
        else
          (
            let _ = Collections.HashSet.insert seen (SourceId.to_int source_id) in
            match source_of_id session source_id with
            | None -> discover rest seen missing
            | Some source ->
                let source_modules = module_dependencies session ~deps_env_key ~deps_env source in
                let opened_source_ids = implicit_open_source_ids source in
                let (unresolved_modules, additional) =
                  source_modules
                  |> List.fold_left
                    (fun (unresolved_modules, additional) module_name ->
                      match local_dependency_source_ids source module_name with
                      | _ :: _ as dependency_source_ids -> (
                        unresolved_modules,
                        dependency_source_ids @ additional
                      )
                      | [] ->
                          if dependency_is_loaded_only module_name then
                            (unresolved_modules, additional)
                          else
                            (module_name :: unresolved_modules, additional))
                    ([], opened_source_ids)
                in
                let dependency_nested_modules =
                  if List.is_empty unresolved_modules then
                    []
                  else
                    source_modules |> List.concat_map
                      (fun module_name ->
                        let local_nested_modules = local_nested_module_prefixes module_name in
                        let loaded_nested_modules =
                          if dependency_is_loaded_only module_name then
                            loaded_nested_module_prefixes module_name
                          else
                            []
                        in
                        local_nested_modules @ loaded_nested_modules) |> fun nested_modules ->
                      nested_modules @ implicit_open_nested_modules source |> List.sort_uniq String.compare
                in
                let updated_missing =
                  unresolved_modules
                  |> List.fold_left
                    (fun missing module_name ->
                      if List.mem module_name dependency_nested_modules then
                        missing
                      else
                        add_missing missing module_name source_id)
                    missing
                in
                discover (additional @ rest) seen updated_missing
          )
  in
  let missing = discover initial_source_ids seen [] in
  MissingRequirements.of_list
    (missing
    |> List.map
      (fun (module_name, requested_by) ->
        MissingRequirements.MissingModuleSummary { module_name; requested_by }))

let local_source_closure = fun session roots ->
  let (deps_env_key, deps_env) = deps_env_for_loaded_modules session session.config.loaded_modules in
  let loaded_module_names = session.config.loaded_modules
  |> LoadedModules.names
  |> Collections.HashSet.of_list in
  let implicit_open_source_ids (source: Source.t) = source.implicit_opens
  |> List.map SurfacePath.to_string
  |> List.concat_map
    (fun module_name ->
      local_source_ids_for_module_in_scope session ~current_module_name:(Source.module_name source) module_name) in
  let local_dependency_source_ids source module_name = local_source_ids_for_module_in_scope
    session
    ~current_module_name:(Source.module_name source)
    module_name in
  let initial_source_ids =
    roots
    |> List.filter_map
      (fun source_id ->
        source_of_id session source_id |> Option.map
          (fun (source: Source.t) ->
            let siblings = source_ids_of_module session (Source.module_name source) in
            if List.is_empty siblings then
              [ source_id ]
            else
              siblings))
    |> List.flatten
    |> List.sort_uniq SourceId.compare
  in
  let seen = Collections.HashSet.with_capacity (List.length initial_source_ids) in
  let rec discover to_visit seen =
    match to_visit with
    | [] -> seen
    | source_id :: rest ->
        if Collections.HashSet.contains seen (SourceId.to_int source_id) then
          discover rest seen
        else
          (
            let _ = Collections.HashSet.insert seen (SourceId.to_int source_id) in
            match source_of_id session source_id with
            | None -> discover rest seen
            | Some source ->
                let additional = (implicit_open_source_ids source)
                @ (module_dependencies session ~deps_env_key ~deps_env source
                |> List.filter
                  (fun module_name ->
                    not
                      (Collections.HashSet.contains loaded_module_names module_name
                      && not (has_local_source_for_module session module_name)))
                |> List.concat_map (local_dependency_source_ids source)) in
                discover (additional @ rest) seen
          )
  in
  let closure_source_ids = discover initial_source_ids seen in
  session.sources |> List.filter
    (fun (source: Source.t) ->
      Collections.HashSet.contains closure_source_ids (SourceId.to_int source.source_id))

let local_module_cycles = fun session roots ->
  let (deps_env_key, deps_env) = deps_env_for_loaded_modules session session.config.loaded_modules in
  let sources = local_source_closure session roots in
  let closure_module_names = sources |> List.map Source.module_name |> List.sort_uniq String.compare in
  let adjacency = Collections.HashMap.with_capacity (List.length closure_module_names) in
  let source_ids_by_module_name = Collections.HashMap.with_capacity
    (List.length closure_module_names) in
  let add_dependency module_name dependency_module_name =
    let existing = Collections.HashMap.get adjacency module_name |> Option.unwrap_or ~default:[] in
    let updated = (dependency_module_name :: existing) |> List.sort_uniq String.compare in
    let _ = Collections.HashMap.insert adjacency module_name updated in
    ()
  in
  List.iter
    (fun (source: Source.t) ->
      let current_module_name = Source.module_name source in
      let existing_source_ids = Collections.HashMap.get source_ids_by_module_name current_module_name
      |> Option.unwrap_or ~default:[] in
      let _ = Collections.HashMap.insert source_ids_by_module_name current_module_name
        ((source.source_id :: existing_source_ids) |> List.sort_uniq SourceId.compare)
      in
      let dependencies = (source.implicit_opens |> List.map SurfacePath.to_string)
      @ module_dependencies session ~deps_env_key ~deps_env source in
      dependencies
      |> List.concat_map
        (fun module_name -> local_source_ids_for_module_in_scope session ~current_module_name module_name)
      |> List.filter_map (source_of_id session)
      |> List.map Source.module_name
      |> List.filter
        (fun module_name ->
          List.mem module_name closure_module_names)
      |> List.iter (add_dependency current_module_name))
    sources;
  let unvisited = 0 in
  let visiting = 1 in
  let done_ = 2 in
  let states = Collections.HashMap.with_capacity (List.length closure_module_names) in
  let cycles = ref [] in
  let seen_cycles = Collections.HashSet.with_capacity 8 in
  let cycle_from_stack target stack =
    let rec loop acc = function
      | [] -> List.rev acc
      | head :: tail ->
          let acc = head :: acc in
          if String.equal head target then
            List.rev acc
          else
            loop acc tail
    in
    loop [] stack
  in
  let add_cycle module_names =
    let module_names = module_names |> List.sort_uniq String.compare in
    let key = String.concat "\x1f" module_names in
    if not (Collections.HashSet.contains seen_cycles key) then
      (
        let _ = Collections.HashSet.insert seen_cycles key in
        let source_ids = module_names
        |> List.concat_map
          (fun module_name ->
            Collections.HashMap.get source_ids_by_module_name module_name
            |> Option.unwrap_or ~default:[])
        |> List.sort_uniq SourceId.compare in
        cycles := MissingRequirements.LocalModuleCycle { module_names; source_ids } :: !cycles
      )
  in
  let rec visit stack module_name =
    let state = Collections.HashMap.get states module_name |> Option.unwrap_or ~default:unvisited in
    if Int.equal state done_ then
      ()
    else if Int.equal state visiting then
      add_cycle (cycle_from_stack module_name stack)
    else
      (
        let _ = Collections.HashMap.insert states module_name visiting in
        let stack = module_name :: stack in
        let dependencies = Collections.HashMap.get adjacency module_name
        |> Option.unwrap_or ~default:[] in
        List.iter (visit stack) dependencies;
        let _ = Collections.HashMap.insert states module_name done_ in
        ()
      )
  in
  List.iter (visit []) closure_module_names;
  List.rev !cycles

let prepare_snapshot = fun session ~roots ->
  let missing_root_source_ids = roots
  |> List.filter (fun root_id -> Option.is_none (source_of_id session root_id)) in
  let missing_roots = missing_root_source_ids
  |> List.map (fun source_id -> MissingRequirements.MissingRootSource { source_id }) in
  TypConfig.emit_event
    session.config
    (fun () ->
        Event.PrepareSnapshotStarted {
          roots;
          root_modules = root_module_names session roots;
          session_source_count = List.length session.sources;
          loaded_module_count = LoadedModules.len session.config.loaded_modules
      });
  let rec hydrate_session session =
    let missing_modules = collect_missing_module_summaries session roots in
    match session.config.store with
    | None -> (session, missing_modules)
    | Some store ->
        let missing_module_names =
          MissingRequirements.requirements missing_modules
          |> List.filter_map
            (
              function
              | MissingRequirements.MissingModuleSummary { module_name; _ } -> Some module_name
              | MissingRequirements.LocalModuleCycle _ -> None
              | MissingRequirements.MissingRootSource _ -> None
            )
          |> List.sort_uniq String.compare
        in
        if List.is_empty missing_module_names then
          ()
        else
          TypConfig.emit_event
            session.config
            (fun () ->
              Event.HydrateModuleTypingsStarted { roots; missing_modules = missing_module_names });
        let hydrated = missing_module_names
        |> List.filter_map (fun module_name -> Store.load_module_typings store ~module_name) in
        let loaded_modules =
          if List.is_empty hydrated then
            session.config.loaded_modules
          else
            merge_loaded_modules session.config.loaded_modules hydrated
        in
        if List.is_empty missing_module_names then
          ()
        else
          TypConfig.emit_event
            session.config
            (fun () ->
              Event.HydrateModuleTypingsFinished {
                roots;
                hydrated_modules = hydrated
                |> List.map ModuleTypings.module_name
                |> List.sort_uniq String.compare;
                loaded_module_count = LoadedModules.len loaded_modules
              });
        if List.is_empty hydrated then
          (session, missing_modules)
        else
          let session = {
            session
            with config = TypConfig.with_loaded_module_index session.config ~loaded_modules
          } in
          hydrate_session session
  in
  let (session, missing_modules) = hydrate_session session in
  let missing_requirements = MissingRequirements.of_list
    (missing_roots @ MissingRequirements.requirements missing_modules) in
  if MissingRequirements.(missing_requirements |> is_empty) then
    let sources = local_source_closure session roots in
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.PrepareSnapshotFinished {
          roots;
          local_source_count = List.length sources;
          loaded_module_count = LoadedModules.len session.config.loaded_modules;
          revision = session.next_revision
        });
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.SnapshotMaterializationStarted {
          roots;
          local_source_count = List.length sources;
          revision = session.next_revision
        });
    let snapshot = Snapshot.make_with_shared_caches
      ~revision:session.next_revision
      ~roots
      ~config:session.config
      ~sources
      ~shared_caches:session.shared_snapshot_caches in
    let module_count = sources
    |> List.map Source.module_name
    |> List.sort_uniq String.compare
    |> List.length in
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.SnapshotMaterializationFinished {
          roots;
          local_source_count = List.length sources;
          module_count;
          revision = session.next_revision
        });
    Ok snapshot
  else (
    TypConfig.emit_event session.config
      (fun () ->
        Event.PrepareSnapshotFailed {
          roots;
          missing_root_source_ids;
          missing_modules =
            MissingRequirements.requirements missing_modules |> List.concat_map
              (
                function
                | MissingRequirements.MissingModuleSummary { module_name; _ } -> [ module_name ]
                | MissingRequirements.LocalModuleCycle _ -> []
                | MissingRequirements.MissingRootSource _ -> []
              ) |> List.sort_uniq String.compare;
        });
    Error missing_requirements
  )

let snapshot = fun session ->
  let roots = session.sources |> List.map (fun (source: Source.t) -> source.source_id) in
  match prepare_snapshot session ~roots with
  | Ok snapshot -> snapshot
  | Error _ -> panic "Session.snapshot: current session sources should always prepare successfully"
