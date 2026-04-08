open Std
open Model
module Snapshot = Snapshot
module SourceAnalysis = SourceAnalysis
module ModulePairing = ModulePairing
module MissingRequirements = MissingRequirements

type t = {
  config: TypConfig.t;
  next_source_id: int;
  next_revision: int;
  sources: Source.t list;
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

let root_module_names = fun session roots ->
  roots
  |> List.filter_map (source_of_id session)
  |> List.map Source.module_name
  |> List.sort_uniq String.compare

let replace_source = fun sources updated ->
  sources |> List.map
    (fun (source: Source.t) ->
      if SourceId.equal source.source_id updated.Source.source_id then
        updated
      else
        source)

let add_module_name_index = fun session ~module_name ~source_id ->
  let source_ids =
    match Collections.HashMap.get session.source_ids_by_module_name module_name with
    | Some source_ids when List.exists (SourceId.equal source_id) source_ids ->
        source_ids
    | Some source_ids ->
        source_ids @ [ source_id ]
    | None ->
        [ source_id ]
  in
  let _ = Collections.HashMap.insert session.source_ids_by_module_name module_name source_ids in
  session

let add_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source in
  add_module_name_index session ~module_name:(Source.module_name source) ~source_id:source.source_id

let update_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source in
  session

let remove_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.remove session.sources_by_id (SourceId.to_int source.source_id) in
  let module_names = Collections.HashMap.keys session.source_ids_by_module_name in
  let () =
    module_names
    |> List.iter
      (fun module_name ->
        let remaining_source_ids = source_ids_of_module session module_name
        |> List.filter
          (fun current_source_id -> not (SourceId.equal current_source_id source.source_id)) in
        if List.is_empty remaining_source_ids then
          let _ = Collections.HashMap.remove session.source_ids_by_module_name module_name in
          ()
        else
          let _ = Collections.HashMap.insert
            session.source_ids_by_module_name
            module_name
            remaining_source_ids in
          ())
  in
  session

let loaded_modules_key = fun loaded_modules ->
  loaded_modules
  |> List.map
    (fun typings ->
      ModuleTypings.module_name typings
      ^ ":"
      ^ (ModuleTypings.source_hash typings |> Crypto.Digest.hex))
  |> List.sort String.compare
  |> String.concat "|"

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
  let rec loop order merged remaining =
    match remaining with
    | [] ->
        order |> List.rev |> List.filter_map
          (fun module_name ->
            List.assoc_opt module_name merged)
    | summary :: tail ->
        let module_name = ModuleTypings.module_name summary in
        let (order, merged) =
          match List.assoc_opt module_name merged with
          | None -> (module_name :: order, (module_name, summary) :: merged)
          | Some _ -> (order, merged)
        in
        loop order merged tail
  in
  loop [] [] (preferred @ fallback)

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
  IdentPath.of_string export_name |> IdentPath.to_segments |> loop []

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
                ~path:((module_name :: segments))
                ~free_names:[ module_name ])
            env
        in
        ModuleTypings.type_decls summary |> List.fold_left
          (fun env (type_decl: FileSummary.type_decl) ->
            if IdentPath.is_empty type_decl.scope_path then
              env
            else
              Syn.Deps.Env.add_path
                env
                ~path:((module_name :: IdentPath.to_segments type_decl.scope_path))
                ~free_names:[ module_name ])
          env
      in
      let env = List.fold_left add_summary_paths Syn.Deps.Env.empty loaded_modules in
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
            implementation.Syn.Cst.items |> List.filter_map
              (fun (item: Syn.Cst.StructureItem.t) ->
                match item with
                | Syn.Cst.StructureItem.ModuleDeclaration declaration -> Some (Syn.Cst.ModuleStructure.name
                  declaration)
                | _ -> None)
        | Syn.Cst.Interface interface ->
            interface.Syn.Cst.items |> List.filter_map
              (fun (item: Syn.Cst.SignatureItem.t) ->
                match item with
                | Syn.Cst.SignatureItem.ModuleDeclaration declaration -> Some (Syn.Cst.ModuleSignature.name
                  declaration)
                | _ -> None)
      in
      let _ = Collections.HashMap.insert session.declared_modules_by_source_hash key modules in
      modules

let module_dependencies = fun session ~deps_env_key ~deps_env (source: Source.t) ->
  let key = deps_env_key ^ ":" ^ source_hash_key source in
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
  |> List.map ModuleTypings.module_name
  |> Collections.HashSet.of_list in
  let implicit_open_module_names (source: Source.t) = source.implicit_opens |> List.map IdentPath.to_string in
  let implicit_open_source_ids source = implicit_open_module_names source
  |> List.concat_map (source_ids_of_module session) in
  let local_nested_module_prefixes module_name = source_ids_of_module session module_name
  |> List.filter_map (source_of_id session)
  |> List.concat_map (declared_modules session)
  |> List.sort_uniq String.compare in
  let loaded_nested_module_prefixes module_name =
    match
      session.config.loaded_modules |> List.find_opt
        (fun summary ->
          String.equal module_name (ModuleTypings.module_name summary))
    with
    | None -> []
    | Some summary ->
        ModuleTypings.exports summary |> List.map fst |> List.filter_map
          (fun export_name ->
            let segments = IdentPath.of_string export_name |> IdentPath.to_segments in
            match segments with
            | head :: _ :: _ when String.length head > 0 && is_uppercase_ascii head.[0] -> Some head
            | _ -> None) |> List.sort_uniq String.compare
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
          let requesters' =
            if List.exists (SourceId.equal requested_by) requesters then
              requesters
            else
              requesters @ [ requested_by ]
          in
          (name, requesters') :: tail
      | head :: tail ->
          head :: loop tail
    in
    loop missing
  in
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
                let dependency_nested_modules =
                  source_modules
                  |> List.concat_map
                    (fun module_name ->
                      let local_nested_modules = local_nested_module_prefixes module_name in
                      let loaded_nested_modules =
                        if Collections.HashSet.contains loaded_module_names module_name then
                          loaded_nested_module_prefixes module_name
                        else
                          []
                      in
                      local_nested_modules @ loaded_nested_modules)
                  |> fun nested_modules ->
                    nested_modules @ implicit_open_nested_modules source |> List.sort_uniq String.compare
                in
                let (missing', additional) =
                  source_modules
                  |> List.fold_left
                    (fun (missing, additional) module_name ->
                      if Collections.HashSet.contains loaded_module_names module_name then
                        (missing, additional)
                      else
                        match source_ids_of_module session module_name with
                        | _ :: _ as dependency_source_ids -> (
                          missing,
                          dependency_source_ids @ additional
                        )
                        | [] ->
                            if List.mem module_name dependency_nested_modules then
                              (missing, additional)
                            else
                              (add_missing missing module_name source_id, additional))
                    (missing, opened_source_ids)
                in
                discover (additional @ rest) seen missing'
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
  |> List.map ModuleTypings.module_name
  |> Collections.HashSet.of_list in
  let implicit_open_source_ids (source: Source.t) = source.implicit_opens
  |> List.map IdentPath.to_string
  |> List.concat_map (source_ids_of_module session) in
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
                    not (Collections.HashSet.contains loaded_module_names module_name))
                |> List.concat_map (source_ids_of_module session)) in
                discover (additional @ rest) seen
          )
  in
  let closure_source_ids = discover initial_source_ids seen in
  session.sources |> List.filter
    (fun (source: Source.t) ->
      Collections.HashSet.contains closure_source_ids (SourceId.to_int source.source_id))

let prepare_snapshot = fun session ~roots ->
  let missing_root_source_ids = roots
  |> List.filter (fun root_id -> Option.is_none (source_of_id session root_id))
  in
  let missing_roots = missing_root_source_ids
  |> List.map (fun source_id -> MissingRequirements.MissingRootSource { source_id }) in
  let () = TypConfig.emit_event session.config
    (fun () ->
      Event.PrepareSnapshotStarted {
        roots;
        root_modules = root_module_names session roots;
        session_source_count = List.length session.sources;
        loaded_module_count = List.length session.config.loaded_modules;
      })
  in
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
              | MissingRequirements.MissingRootSource _ -> None
            )
          |> List.sort_uniq String.compare
        in
        let () =
          if List.is_empty missing_module_names then
            ()
          else
            TypConfig.emit_event session.config
              (fun () ->
                Event.HydrateModuleTypingsStarted {
                  roots;
                  missing_modules = missing_module_names;
                })
        in
        let hydrated = missing_module_names
        |> List.filter_map (fun module_name -> Store.load_module_typings store ~module_name) in
        let loaded_modules =
          if List.is_empty hydrated then
            session.config.loaded_modules
          else
            merge_loaded_modules session.config.loaded_modules hydrated in
        let () =
          if List.is_empty missing_module_names then
            ()
          else
            TypConfig.emit_event session.config
              (fun () ->
                Event.HydrateModuleTypingsFinished {
                  roots;
                  hydrated_modules =
                    hydrated
                    |> List.map ModuleTypings.module_name
                    |> List.sort_uniq String.compare;
                  loaded_module_count = List.length loaded_modules;
                })
        in
        if List.is_empty hydrated then
          (session, missing_modules)
        else
          let session = {
            session
            with config = TypConfig.with_loaded_modules session.config ~loaded_modules
          } in
          hydrate_session session
  in
  let (session, missing_modules) = hydrate_session session in
  let missing_requirements = MissingRequirements.of_list
    (missing_roots @ MissingRequirements.requirements missing_modules) in
  if MissingRequirements.(missing_requirements |> is_empty) then
    let sources = local_source_closure session roots in
    let () = TypConfig.emit_event session.config
      (fun () ->
        Event.PrepareSnapshotFinished {
          roots;
          local_source_count = List.length sources;
          loaded_module_count = List.length session.config.loaded_modules;
          revision = session.next_revision;
        })
    in
    Ok (Snapshot.make ~revision:session.next_revision ~roots ~config:session.config ~sources)
  else
    let () = TypConfig.emit_event session.config
      (fun () ->
        Event.PrepareSnapshotFailed {
          roots;
          missing_root_source_ids;
          missing_modules =
            MissingRequirements.requirements missing_modules
            |> List.filter_map
              (
                function
                | MissingRequirements.MissingModuleSummary { module_name; _ } -> Some module_name
                | MissingRequirements.MissingRootSource _ -> None
              )
            |> List.sort_uniq String.compare;
        })
    in
    Error missing_requirements

let snapshot = fun session ->
  let roots = session.sources |> List.map (fun (source: Source.t) -> source.source_id) in
  match prepare_snapshot session ~roots with
  | Ok snapshot -> snapshot
  | Error _ -> panic "Session.snapshot: current session sources should always prepare successfully"
