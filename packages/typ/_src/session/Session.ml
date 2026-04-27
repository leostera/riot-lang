open Std
open Model

module Array = Collections.Array
module Snapshot = Snapshot
module MissingRequirements = MissingRequirements

type t = {
  config: TypConfig.t;
  next_source_id: int;
  next_revision: int;
  sources: Source.t list;
  shared_snapshot_caches: Snapshot.SharedCaches.t;
  sources_by_id: (int, Source.t) Collections.HashMap.t;
  source_ids_by_module_name: (string, SourceId.t list) Collections.HashMap.t;
  module_names_by_source_id: (int, string list) Collections.HashMap.t;
  parse_results_by_source_hash: (string, Syn.Parser.parse_result) Collections.HashMap.t;
  declared_modules_by_source_hash: (string, string list) Collections.HashMap.t;
  top_level_includes_by_source_hash: (string, string list) Collections.HashMap.t;
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
    module_names_by_source_id = Collections.HashMap.with_capacity 64;
    parse_results_by_source_hash = Collections.HashMap.with_capacity 64;
    declared_modules_by_source_hash = Collections.HashMap.with_capacity 64;
    top_level_includes_by_source_hash = Collections.HashMap.with_capacity 64;
    module_dependencies_by_env_and_source_hash = Collections.HashMap.with_capacity 256;
    deps_envs_by_loaded_modules = Collections.HashMap.with_capacity 16;
  }

let config = fun session -> session.config

let with_config = fun session ~config -> { session with config }

let source_hash_key = fun (source: Source.t) ->
  Source.input_hash source
  |> Crypto.Digest.hex

let filename_of_source = fun (source: Source.t) ->
  match source.origin with
  | Source.Path path -> path
  | Source.Label label ->
      Path.of_string label
      |> Result.unwrap_or ~default:(Path.v "<fragment>")

let source_of_id = fun session source_id ->
  Collections.HashMap.get
    session.sources_by_id
    (SourceId.to_int source_id)

let source_ids_of_module = fun session module_name ->
  match Collections.HashMap.get session.source_ids_by_module_name module_name with
  | Some source_ids -> source_ids
  | None -> []

let module_names_of_source_id = fun session source_id ->
  match Collections.HashMap.get session.module_names_by_source_id (SourceId.to_int source_id) with
  | Some module_names -> module_names
  | None -> []

let module_name_index_keys = fun (source: Source.t) ->
  let internal_name = LocalModules.InternalName.of_string (Source.module_name source) in
  (Source.module_name source)
  :: (
    LocalModules.local_module_aliases_of_internal_name internal_name
    |> List.map LocalModules.AmbientName.to_string
  )
  |> List.sort_uniq String.compare

let local_source_ids_for_module = fun session module_name ->
  source_ids_of_module
    session
    module_name

let local_source_ids_for_module_in_scope = fun session ~current_module_name module_name ->
  let current_module_name = LocalModules.InternalName.of_string current_module_name in
  let required_module_name = LocalModules.RequiredName.of_string module_name in
  let matching_sources =
    source_ids_of_module session module_name
    |> List.filter_map (source_of_id session)
    |> List.filter_map
      (fun (source: Source.t) ->
        LocalModules.contextual_match_depth
          ~current_module_name
          ~required_module_name
          ~candidate_module_name:(LocalModules.InternalName.of_string (Source.module_name source))
        |> Option.map (fun depth -> (source.source_id, depth)))
  in
  let best_depth =
    matching_sources
    |> List.fold_left
      (fun best (_, depth) ->
        Some (
          Option.unwrap_or ~default:depth best
          |> Int.max depth
        ))
      None
  in
  match best_depth with
  | None -> []
  | Some best_depth ->
      matching_sources
      |> List.filter_map
        (fun (source_id, depth) ->
          if Int.equal depth best_depth then
            Some source_id
          else
            None)
      |> List.sort_uniq SourceId.compare

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
  sources
  |> List.map
    (fun (source: Source.t) ->
      if SourceId.equal source.source_id Source.(updated.source_id) then
        updated
      else
        source)

let register_module_name = fun session ~module_name ~source_id ->
  let source_ids =
    match Collections.HashMap.get session.source_ids_by_module_name module_name with
    | Some source_ids when List.exists (SourceId.equal source_id) source_ids -> source_ids
    | Some source_ids -> source_ids @ [ source_id ]
    | None -> [ source_id ]
  in
  let _ = Collections.HashMap.insert session.source_ids_by_module_name module_name source_ids in
  let module_names =
    match Collections.HashMap.get session.module_names_by_source_id (SourceId.to_int source_id) with
    | Some module_names when List.mem module_name module_names -> module_names
    | Some module_names -> module_names @ [ module_name ]
    | None -> [ module_name ]
  in
  let _ =
    Collections.HashMap.insert
      session.module_names_by_source_id
      (SourceId.to_int source_id)
      module_names
  in
  session

let add_source_indexes = fun session (source: Source.t) ->
  let _ =
    Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source
  in
  module_name_index_keys source
  |> List.fold_left
    (fun session module_name ->
      register_module_name
        session
        ~module_name
        ~source_id:source.source_id)
    session

let update_source_indexes = fun session (source: Source.t) ->
  let _ =
    Collections.HashMap.insert session.sources_by_id (SourceId.to_int source.source_id) source
  in
  session

let remove_source_indexes = fun session (source: Source.t) ->
  let _ = Collections.HashMap.remove session.sources_by_id (SourceId.to_int source.source_id) in
  let module_names = module_names_of_source_id session source.source_id in
  let _ =
    Collections.HashMap.remove session.module_names_by_source_id (SourceId.to_int source.source_id)
  in
  module_names
  |> List.iter
    (fun module_name ->
      let remaining_source_ids =
        source_ids_of_module session module_name
        |> List.filter
          (fun current_source_id -> not (SourceId.equal current_source_id source.source_id))
      in
      if List.is_empty remaining_source_ids then
        let _ = Collections.HashMap.remove session.source_ids_by_module_name module_name in
        ()
      else
        let _ =
          Collections.HashMap.insert
            session.source_ids_by_module_name
            module_name
            remaining_source_ids
        in
        ());
  session

let loaded_modules_key = fun loaded_modules -> LoadedModules.stable_key loaded_modules

let visible_module_names_of_source = fun session (source: Source.t) ->
  let internal_name = LocalModules.InternalName.of_string (Source.module_name source) in
  LocalModuleGraph.InternalName internal_name
  :: (
    module_names_of_source_id session source.source_id
    |> List.map
      (fun module_name ->
        LocalModuleGraph.AmbientName (LocalModules.AmbientName.of_string module_name))
  )

let create_source = fun
  session
  ~kind
  ~module_name
  ~implicit_opens
  ~origin
  ~source_hash
  ~parse_result
  ~cst ->
  let source_id = SourceId.of_int session.next_source_id in
  let source =
    Source.make_prepared
      ~source_id
      ~kind
      ~module_name
      ~implicit_opens
      ~origin
      ~revision:session.next_revision
      ~source_hash
      ~parse_result
      ~cst
  in
  let session =
    {
      session with
      next_source_id = session.next_source_id + 1;
      next_revision = session.next_revision + 1;
      sources = session.sources @ [ source ];
    }
    |> fun session -> add_source_indexes session source
  in
  (session, source_id)

let register_source_alias = fun session source_id ~module_name ->
  match source_of_id session source_id with
  | None -> session
  | Some _ -> register_module_name session ~module_name ~source_id

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
      let updated =
        Source.make_prepared
          ~source_id:source.source_id
          ~kind:source.kind
          ~module_name:source.module_name
          ~implicit_opens:source.implicit_opens
          ~origin:source.origin
          ~revision
          ~source_hash
          ~parse_result
          ~cst
      in
      {
        session with
        next_revision = revision + 1;
        sources = replace_source session.sources updated;
      }
      |> fun session -> update_source_indexes session updated

let remove_source = fun session source_id ->
  match source_of_id session source_id with
  | None -> { session with next_revision = session.next_revision + 1 }
  | Some source ->
      {
        session with
        next_revision = session.next_revision + 1;
        sources = List.filter
          (fun (current_source: Source.t) -> not (SourceId.equal current_source.source_id source_id))
          session.sources;
      }
      |> fun session -> remove_source_indexes session source

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let module_segments_of_export_name = fun export_name ->
  let rec loop acc = function
    | segment :: rest when String.length segment > 0 && is_uppercase_ascii segment.[0] ->
        loop (segment :: acc) rest
    | _ -> List.rev acc
  in
  SurfacePath.to_segments export_name
  |> loop []

let nested_module_prefixes_of_surface = fun ~exports ~type_decls ->
  let export_prefixes =
    exports
    |> List.map fst
    |> List.filter_map
      (fun export_name ->
        match module_segments_of_export_name export_name with
        | head :: _ -> Some head
        | [] -> None)
  in
  let type_decl_prefixes =
    type_decls
    |> List.filter_map
      (fun (type_decl: FileSummary.type_decl) ->
        match SurfacePath.to_segments type_decl.scope_path with
        | head :: _ when String.length head > 0 && is_uppercase_ascii head.[0] -> Some head
        | _ -> None)
  in
  export_prefixes @ type_decl_prefixes
  |> List.sort_uniq String.compare

let nested_module_prefixes_of_typings = fun module_typings ->
  nested_module_prefixes_of_surface
    ~exports:(ModuleTypings.exports module_typings)
    ~type_decls:(ModuleTypings.type_decls module_typings)

let nested_module_prefixes_of_compiled_scope = fun compiled_scope ->
  nested_module_prefixes_of_surface
    ~exports:(CompiledScope.exports compiled_scope)
    ~type_decls:(CompiledScope.type_decls compiled_scope)

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
              | segments ->
                  Syn.Deps.Env.add_path
                    env
                    ~path:(module_name :: segments)
                    ~free_names:[ module_name ])
            env
        in
        ModuleTypings.type_decls summary
        |> List.fold_left
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
      let env =
        LoadedModules.fold
          (fun _required_name summary env -> add_summary_paths env summary)
          loaded_modules
          Syn.Deps.Env.empty
      in
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
            Syn.Cst.(implementation.items)
            |> List.filter_map
              (fun (item: Syn.Cst.StructureItem.t) ->
                match item with
                | Syn.Cst.StructureItem.ModuleDeclaration declaration ->
                    Some (Syn.Cst.ModuleStructure.name declaration)
                | _ -> None)
        | Syn.Cst.Interface interface ->
            Syn.Cst.(interface.items)
            |> List.filter_map
              (fun (item: Syn.Cst.SignatureItem.t) ->
                match item with
                | Syn.Cst.SignatureItem.ModuleDeclaration declaration ->
                    Some (Syn.Cst.ModuleSignature.name declaration)
                | _ -> None)
      in
      let _ = Collections.HashMap.insert session.declared_modules_by_source_hash key modules in
      modules

let module_path_of_ident = fun ident ->
  Syn.Cst.Ident.segments ident
  |> List.map Syn.Cst.Token.text
  |> String.concat "."

let rec module_path_of_module_expression = function
  | Syn.Cst.ModuleExpression.Path path -> Some (module_path_of_ident path)
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } -> module_path_of_module_expression inner
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ } ->
      module_path_of_module_expression module_expression
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } ->
      module_path_of_module_expression module_expression
  | Syn.Cst.ModuleExpression.Structure _
  | Syn.Cst.ModuleExpression.Functor _
  | Syn.Cst.ModuleExpression.Apply _
  | Syn.Cst.ModuleExpression.ApplyUnit _
  | Syn.Cst.ModuleExpression.ModuleUnpack _
  | Syn.Cst.ModuleExpression.Extension _ -> None

let rec module_path_of_module_type = function
  | Syn.Cst.ModuleType.TypeOf { module_path; _ } -> Some (module_path_of_ident module_path)
  | Syn.Cst.ModuleType.Parenthesized { inner; _ } -> module_path_of_module_type inner
  | Syn.Cst.ModuleType.With { base; _ } -> module_path_of_module_type base
  | Syn.Cst.ModuleType.Attribute { module_type; _ } -> module_path_of_module_type module_type
  | Syn.Cst.ModuleType.Path _
  | Syn.Cst.ModuleType.Signature _
  | Syn.Cst.ModuleType.Functor _
  | Syn.Cst.ModuleType.Extension _ -> None

let top_level_include_module_paths = fun session (source: Source.t) ->
  let key = source_hash_key source in
  match Collections.HashMap.get session.top_level_includes_by_source_hash key with
  | Some module_paths -> module_paths
  | None ->
      let include_path_of_target = function
        | Syn.Cst.ModuleExpression module_expression ->
            module_path_of_module_expression module_expression
        | Syn.Cst.ModuleType module_type -> module_path_of_module_type module_type
      in
      let module_paths =
        match source.cst with
        | Syn.Cst.Implementation implementation ->
            Syn.Cst.(implementation.items)
            |> List.filter_map
              (
                function
                | Syn.Cst.StructureItem.IncludeStatement include_statement ->
                    include_path_of_target include_statement.target
                | _ -> None
              )
        | Syn.Cst.Interface interface ->
            Syn.Cst.(interface.items)
            |> List.filter_map
              (
                function
                | Syn.Cst.SignatureItem.IncludeStatement include_statement ->
                    include_path_of_target include_statement.target
                | _ -> None
              )
      in
      let module_paths = List.sort_uniq String.compare module_paths in
      let _ =
        Collections.HashMap.insert session.top_level_includes_by_source_hash key module_paths
      in
      module_paths

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
      let _ =
        Collections.HashMap.insert session.module_dependencies_by_env_and_source_hash key modules
      in
      modules

let dedupe_strings_preserving_order = fun module_names ->
  let seen = Collections.HashSet.with_capacity (List.length module_names + 1) in
  module_names
  |> List.filter
    (fun module_name ->
      if Collections.HashSet.contains seen module_name then
        false
      else
        let _ = Collections.HashSet.insert seen module_name in
        true)

let local_required_names_of_source = fun session ~deps_env_key ~deps_env (source: Source.t) ->
  let current_module_name = LocalModules.InternalName.of_string (Source.module_name source) in
  let implicit_open_modules =
    source.implicit_opens
    |> List.map SurfacePath.to_string
    |> List.filter
      (fun module_name ->
        LocalModules.should_include_implicit_open
          ~current_module_name
          ~module_name)
  in
  module_dependencies session ~deps_env_key ~deps_env source @ implicit_open_modules
  |> dedupe_strings_preserving_order
  |> List.map LocalModules.RequiredName.of_string

let local_module_graph = fun session ->
  let (deps_env_key, deps_env) =
    deps_env_for_loaded_modules session session.config.loaded_modules
  in
  LocalModuleGraph.create
    ~ordered_sources:(
      session.sources
      |> List.map
        (fun (source: Source.t) ->
          let internal_name = LocalModules.InternalName.of_string (Source.module_name source) in
          {
            LocalModuleGraph.payload = source;
            source_id = source.source_id;
            internal_name;
            visible_names = visible_module_names_of_source session source;
            required_names = local_required_names_of_source session ~deps_env_key ~deps_env source;
          })
    )

let collect_missing_module_summaries = fun session ~closure_sources ->
  let (deps_env_key, deps_env) =
    deps_env_for_loaded_modules session session.config.loaded_modules
  in
  let loaded_module_names =
    session.config.loaded_modules
    |> LoadedModules.names
    |> Collections.HashSet.of_list
  in
  let closure_source_ids =
    closure_sources
    |> List.map (fun (source: Source.t) -> source.source_id)
    |> Collections.HashSet.of_list
  in
  let implicit_open_module_names (source: Source.t) =
    source.implicit_opens
    |> List.map SurfacePath.to_string
  in
  let local_dependency_source_ids source module_name =
    local_source_ids_for_module_in_scope
      session
      ~current_module_name:(Source.module_name source)
      module_name
    |> List.filter (fun source_id -> Collections.HashSet.contains closure_source_ids source_id)
  in
  let dependency_is_loaded_only module_name =
    Collections.HashSet.contains
      loaded_module_names
      (LocalModules.RequiredName.of_string module_name)
    && not (has_local_source_for_module session module_name)
  in
  let local_nested_module_prefixes_cache = Collections.HashMap.with_capacity 32 in
  let source_ids_in_closure_for_module module_name =
    source_ids_of_module session module_name
    |> List.filter (fun source_id -> Collections.HashSet.contains closure_source_ids source_id)
  in
  let loaded_nested_module_prefixes module_name =
    let module_path = SurfacePath.of_string module_name in
    match SurfacePath.uncons module_path with
    | None -> []
    | Some (head, suffix) -> (
        match LoadedModules.get
          session.config.loaded_modules
          ~required_name:(LocalModules.RequiredName.of_string head) with
        | None -> []
        | Some summary ->
            let root_scope = ModuleTypings.compiled_scope summary in
            let target_scope =
              if SurfacePath.is_empty suffix then
                Some root_scope
              else
                CompiledScope.lookup_module root_scope suffix
            in
            target_scope
            |> Option.map nested_module_prefixes_of_compiled_scope
            |> Option.unwrap_or ~default:[]
      )
  in
  let resolve_local_module_names ~current_module_name module_name =
    local_source_ids_for_module_in_scope session ~current_module_name module_name
    |> List.filter (fun source_id -> Collections.HashSet.contains closure_source_ids source_id)
    |> List.filter_map (source_of_id session)
    |> List.map Source.module_name
    |> List.sort_uniq String.compare
  in
  let rec nested_module_prefixes_of_module_path ~current_module_name module_name =
    match resolve_local_module_names ~current_module_name module_name with
    | [] -> loaded_nested_module_prefixes module_name
    | local_module_names ->
        local_module_names
        |> List.concat_map local_nested_module_prefixes_of_internal_name
        |> List.sort_uniq String.compare
  and local_nested_module_prefixes_of_internal_name module_name =
    match Collections.HashMap.get local_nested_module_prefixes_cache module_name with
    | Some nested_modules -> nested_modules
    | None ->
        let _ = Collections.HashMap.insert local_nested_module_prefixes_cache module_name [] in
        let module_sources =
          source_ids_in_closure_for_module module_name
          |> List.filter_map (source_of_id session)
        in
        let declared_nested_modules =
          module_sources
          |> List.concat_map (declared_modules session)
        in
        let included_nested_modules =
          module_sources
          |> List.concat_map
            (fun (source: Source.t) ->
              top_level_include_module_paths session source
              |> List.concat_map
                (nested_module_prefixes_of_module_path ~current_module_name:source.module_name))
        in
        let nested_modules =
          declared_nested_modules @ included_nested_modules
          |> List.sort_uniq String.compare
        in
        let _ =
          Collections.HashMap.insert local_nested_module_prefixes_cache module_name nested_modules
        in
        nested_modules
  in
  let implicit_open_nested_modules source =
    implicit_open_module_names source
    |> List.concat_map
      (nested_module_prefixes_of_module_path ~current_module_name:(Source.module_name source))
    |> List.sort_uniq String.compare
  in
  let rec add_missing missing module_name requested_by =
    let rec loop = function
      | [] -> [ (module_name, [ requested_by ]); ]
      | (name, requesters) :: tail when String.equal name module_name ->
          let updated_requesters =
            if List.exists (SourceId.equal requested_by) requesters then
              requesters
            else
              requesters @ [ requested_by ]
          in
          (name, updated_requesters) :: tail
      | head :: tail -> head :: loop tail
    in
    loop missing
  in
  let missing =
    closure_sources
    |> List.fold_left
      (fun missing (source: Source.t) ->
        let source_modules = module_dependencies session ~deps_env_key ~deps_env source in
        let unresolved_modules =
          source_modules
          |> List.filter
            (fun module_name ->
              match local_dependency_source_ids source module_name with
              | _ :: _ -> false
              | [] -> not (dependency_is_loaded_only module_name))
        in
        let dependency_nested_modules =
          if List.is_empty unresolved_modules then
            []
          else
            source_modules
            |> List.concat_map
              (nested_module_prefixes_of_module_path
                ~current_module_name:(Source.module_name source))
            |> fun nested_modules ->
              nested_modules @ implicit_open_nested_modules source
              |> List.sort_uniq String.compare
        in
        unresolved_modules
        |> List.fold_left
          (fun missing module_name ->
            if List.mem module_name dependency_nested_modules then
              missing
            else
              add_missing missing module_name source.source_id)
          missing)
      []
  in
  MissingRequirements.of_list
    (
      missing
      |> List.map
        (fun (module_name, requested_by) ->
          MissingRequirements.MissingModuleSummary { module_name; requested_by })
    )

let local_source_closure = fun session ~graph ~roots ->
  let closure_source_ids =
    LocalModuleGraph.closure_source_ids graph ~roots
    |> Collections.HashSet.of_list
  in
  session.sources
  |> List.filter
    (fun (source: Source.t) -> Collections.HashSet.contains closure_source_ids source.source_id)

let local_module_cycles = fun ~(graph:Source.t LocalModuleGraph.t) ~roots ->
  let cycle_relevant_group (group: Source.t LocalModuleGraph.group) =
    group.sources
    |> List.exists
      (fun (source: Source.t LocalModuleGraph.graph_source) ->
        match source.input.payload.kind with
        | Source.Generated -> false
        | Source.File
        | Source.Fragment -> true)
  in
  let relevant_group_ids =
    LocalModuleGraph.closure_group_ids graph ~roots
    |> List.filter (fun group_id -> cycle_relevant_group graph.groups.(group_id))
  in
  match LocalModuleGraph.ordered_subset_group_ids graph ~group_ids:relevant_group_ids with
  | Ok _ -> []
  | Error cycle ->
      [
        MissingRequirements.LocalModuleCycle {
          module_names = cycle.module_names;
          source_ids = cycle.source_ids;
        };
      ]

let prepare_snapshot = fun session ~roots ->
  let missing_root_source_ids =
    roots
    |> List.filter (fun root_id -> Option.is_none (source_of_id session root_id))
  in
  let missing_roots =
    missing_root_source_ids
    |> List.map (fun source_id -> MissingRequirements.MissingRootSource { source_id })
  in
  let graph = local_module_graph session in
  let cycle_requirements = local_module_cycles ~graph ~roots in
  TypConfig.emit_event
    session.config
    (fun () ->
      Event.PrepareSnapshotStarted {
        roots;
        root_modules = root_module_names session roots;
        session_source_count = List.length session.sources;
        loaded_module_count = LoadedModules.len session.config.loaded_modules;
      });
  let missing_requirements =
    if List.is_empty cycle_requirements then
      let closure_sources = local_source_closure session ~graph ~roots in
      let rec hydrate_session session =
        let missing_modules = collect_missing_module_summaries session ~closure_sources in
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
                  Event.HydrateModuleTypingsStarted {
                    roots;
                    missing_modules = missing_module_names;
                  });
            let hydrated =
              missing_module_names
              |> List.filter_map (fun module_name -> Store.load_module_typings store ~module_name)
            in
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
                    hydrated_modules =
                      hydrated
                      |> List.map ModuleTypings.module_name
                      |> List.sort_uniq String.compare;
                    loaded_module_count = LoadedModules.len loaded_modules;
                  });
            if List.is_empty hydrated then
              (session, missing_modules)
            else
              let session =
                {
                  session with
                  config = TypConfig.with_loaded_module_index session.config ~loaded_modules;
                }
              in
              hydrate_session session
      in
      let (session, missing_modules) = hydrate_session session in
      let missing_requirements =
        MissingRequirements.of_list
          (missing_roots @ MissingRequirements.requirements missing_modules)
      in
      (session, missing_requirements)
    else
      (session, MissingRequirements.of_list (missing_roots @ cycle_requirements))
  in
  let (session, missing_requirements) = missing_requirements in
  if MissingRequirements.(missing_requirements
  |> is_empty) then
    let sources = local_source_closure session ~graph ~roots in
    let snapshot_config = TypConfig.with_store session.config ~store:None in
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.PrepareSnapshotFinished {
          roots;
          local_source_count = List.length sources;
          loaded_module_count = LoadedModules.len session.config.loaded_modules;
          revision = session.next_revision;
        });
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.SnapshotMaterializationStarted {
          roots;
          local_source_count = List.length sources;
          revision = session.next_revision;
        });
    let snapshot =
      Snapshot.make_with_shared_caches
        ~revision:session.next_revision
        ~roots
        ~config:snapshot_config
        ~sources
        ~shared_caches:session.shared_snapshot_caches
    in
    let module_count =
      sources
      |> List.map Source.module_name
      |> List.sort_uniq String.compare
      |> List.length
    in
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.SnapshotMaterializationFinished {
          roots;
          local_source_count = List.length sources;
          module_count;
          revision = session.next_revision;
        });
    Ok snapshot
  else (
    TypConfig.emit_event
      session.config
      (fun () ->
        Event.PrepareSnapshotFailed {
          roots;
          missing_root_source_ids;
          missing_modules =
            MissingRequirements.requirements missing_requirements
            |> List.concat_map
              (
                function
                | MissingRequirements.MissingModuleSummary { module_name; _ } -> [ module_name ]
                | MissingRequirements.LocalModuleCycle _ -> []
                | MissingRequirements.MissingRootSource _ -> []
              )
            |> List.sort_uniq String.compare;
        });
    Error missing_requirements
  )

let snapshot = fun session ->
  let roots =
    session.sources
    |> List.map (fun (source: Source.t) -> source.source_id)
  in
  match prepare_snapshot session ~roots with
  | Ok snapshot -> snapshot
  | Error _ -> panic "Session.snapshot: current session sources should always prepare successfully"
