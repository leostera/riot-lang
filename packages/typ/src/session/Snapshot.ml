open Std
open Infer
open Model
module LocalModules = LocalModules
module ModuleSurface = ModuleSurface

type analysis_state =
  | NotStarted
  | InProgress
  | Finished of SourceAnalysis.t

type analysis_slot = {
  source_id: SourceId.t;
  source: Source.t;
  config: TypConfig.t;
  mutable analysis: SourceAnalysis.t option;
  mutable state: analysis_state;
}

type shared_cache_namespace = string

type shared_module_result_cache_key = string

and source_analysis_cache_key = string

and loaded_ambient_cache_key = string

and local_ambient_cache_key = string

and ambient_surface_cache_key = string

module SharedCaches = struct
  type t = {
    module_result_cache: (shared_module_result_cache_key, ModulePairing.t) Collections.HashMap.t;
    source_analysis_cache: (source_analysis_cache_key, SourceAnalysis.t) Collections.HashMap.t;
    initial_env_cache: (ambient_surface_cache_key, Infer.Env.t) Collections.HashMap.t;
    loaded_ambient_env_cache:
      (loaded_ambient_cache_key, (IdentPath.t * TypeScheme.t) list) Collections.HashMap.t;
    loaded_ambient_type_decls_cache:
      (loaded_ambient_cache_key, FileSummary.type_decl list) Collections.HashMap.t;
    local_ambient_env_cache:
      (local_ambient_cache_key, (IdentPath.t * TypeScheme.t) list) Collections.HashMap.t;
    local_ambient_type_decls_cache:
      (local_ambient_cache_key, FileSummary.type_decl list) Collections.HashMap.t;
  }

  let create = fun () ->
    {
      module_result_cache = Collections.HashMap.with_capacity 128;
      source_analysis_cache = Collections.HashMap.with_capacity 256;
      initial_env_cache = Collections.HashMap.with_capacity 256;
      loaded_ambient_env_cache = Collections.HashMap.with_capacity 64;
      loaded_ambient_type_decls_cache = Collections.HashMap.with_capacity 64;
      local_ambient_env_cache = Collections.HashMap.with_capacity 256;
      local_ambient_type_decls_cache = Collections.HashMap.with_capacity 256;
    }
end

type t = {
  revision: int;
  roots: SourceId.t list;
  cache_namespace_key: shared_cache_namespace;
  analyses: analysis_slot list;
  module_names: string list;
  module_slots_by_name: (string, analysis_slot list) Collections.HashMap.t;
  module_results_cache: (string, ModulePairing.t) Collections.HashMap.t;
  shared_module_result_cache: (shared_module_result_cache_key, ModulePairing.t) Collections.HashMap.t;
  shared_source_analysis_cache: (source_analysis_cache_key, SourceAnalysis.t) Collections.HashMap.t;
  shared_initial_env_cache: (ambient_surface_cache_key, Infer.Env.t) Collections.HashMap.t;
  shared_loaded_ambient_env_cache:
    (loaded_ambient_cache_key, (IdentPath.t * TypeScheme.t) list) Collections.HashMap.t;
  shared_loaded_ambient_type_decls_cache:
    (loaded_ambient_cache_key, FileSummary.type_decl list) Collections.HashMap.t;
  shared_local_ambient_env_cache:
    (local_ambient_cache_key, (IdentPath.t * TypeScheme.t) list) Collections.HashMap.t;
  shared_local_ambient_type_decls_cache:
    (local_ambient_cache_key, FileSummary.type_decl list) Collections.HashMap.t;
  source_analysis_keys_cache: (int, source_analysis_cache_key option) Collections.HashMap.t;
  module_result_keys_cache: (string, shared_module_result_cache_key option) Collections.HashMap.t;
  local_module_names_cache: (int, string list) Collections.HashMap.t;
  local_ambient_keys_cache: (int, local_ambient_cache_key) Collections.HashMap.t;
  ambient_surface_keys_cache: (int, ambient_surface_cache_key) Collections.HashMap.t;
}

let export_status_of_file_summary = fun summary ->
  match FileSummary.export_status summary with
  | FileSummary.Trusted -> Event.TrustedExport
  | FileSummary.Errored -> Event.ErroredExport
  | FileSummary.Missing -> Event.MissingExport

let export_status_of_module_typings = fun module_typings ->
  match ModuleTypings.export_status module_typings with
  | FileSummary.Trusted -> Event.TrustedExport
  | FileSummary.Errored -> Event.ErroredExport
  | FileSummary.Missing -> Event.MissingExport

let signature_mismatch_subject mismatch =
  match mismatch with
  | Diagnostic.MissingValue { name }
  | Diagnostic.ValueTypeMismatch { name; _ } -> format Format.[ str "value "; str name ]
  | Diagnostic.MissingTypeDeclaration { name }
  | Diagnostic.TypeDeclarationMismatch { name; _ } -> format Format.[ str "type "; str name ]

let signature_mismatch_message = Diagnostic.signature_mismatch_message

let digest_key = fun parts -> parts |> String.concat "\x1f" |> Crypto.hash_string |> Crypto.Digest.hex

let cache_namespace_key_of_config = fun (config: TypConfig.t) ->
  let loaded_modules =
    config.loaded_modules
    |> List.map
      (fun module_typings ->
        format
          Format.[ str (ModuleTypings.module_name module_typings); str ":"; str
              (ModuleTypings.source_hash module_typings |> Crypto.Digest.hex); ])
    |> List.sort String.compare
  in
  digest_key
    (
      [ if config.capture_traces then
          "capture=1"
        else
          "capture=0" ] @ loaded_modules
    )

let make_with_shared_caches = fun ~revision ~roots ~config ~sources ~shared_caches ->
  let analyses =
    sources
    |> List.map
      (fun (source: Source.t) ->
        {
          source_id = source.source_id;
          source;
          config;
          analysis = None;
          state = NotStarted;
        })
  in
  let module_slots_by_name = Collections.HashMap.with_capacity 64 in
  let module_names =
    analyses
    |> List.fold_left
      (fun names (slot: analysis_slot) ->
        let module_name = Source.module_name slot.source in
        let existing = Collections.HashMap.get module_slots_by_name module_name
        |> Option.unwrap_or ~default:[] in
        let _ = Collections.HashMap.insert module_slots_by_name module_name (slot :: existing) in
        if List.mem module_name names then
          names
        else
          module_name :: names)
      []
    |> List.rev
  in
  let () =
    module_names
    |> List.iter
      (fun module_name ->
        match Collections.HashMap.get module_slots_by_name module_name with
        | Some slots ->
            let _ = Collections.HashMap.insert module_slots_by_name module_name (List.rev slots) in
            ()
        | None -> ())
  in
  {
    revision;
    roots;
    cache_namespace_key = cache_namespace_key_of_config config;
    analyses;
    module_names;
    module_slots_by_name;
    module_results_cache = Collections.HashMap.with_capacity 32;
    shared_module_result_cache =
      SharedCaches.(shared_caches.module_result_cache);
    shared_source_analysis_cache =
      SharedCaches.(shared_caches.source_analysis_cache);
    shared_initial_env_cache =
      SharedCaches.(shared_caches.initial_env_cache);
    shared_loaded_ambient_env_cache =
      SharedCaches.(shared_caches.loaded_ambient_env_cache);
    shared_loaded_ambient_type_decls_cache =
      SharedCaches.(shared_caches.loaded_ambient_type_decls_cache);
    shared_local_ambient_env_cache =
      SharedCaches.(shared_caches.local_ambient_env_cache);
    shared_local_ambient_type_decls_cache =
      SharedCaches.(shared_caches.local_ambient_type_decls_cache);
    source_analysis_keys_cache = Collections.HashMap.with_capacity 64;
    module_result_keys_cache = Collections.HashMap.with_capacity 32;
    local_module_names_cache = Collections.HashMap.with_capacity 64;
    local_ambient_keys_cache = Collections.HashMap.with_capacity 64;
    ambient_surface_keys_cache = Collections.HashMap.with_capacity 64;
  }

let make = fun ~revision ~roots ~config ~sources ->
  make_with_shared_caches ~revision ~roots ~config ~sources ~shared_caches:(SharedCaches.create ())

let qualify_exports = fun module_name type_decls exports ->
  ModuleSurface.qualify_exports ~module_name ~type_decls exports

let qualify_type_decls = fun module_name type_decls ->
  ModuleSurface.qualify_type_decls ~module_name type_decls

let module_names_of_slots = fun slots ->
  let rec loop order seen = function
    | [] -> List.rev order
    | (slot: analysis_slot) :: tail ->
        let module_name = Source.module_name slot.source in
        if List.mem module_name seen then
          loop order seen tail
        else
          loop (module_name :: order) (module_name :: seen) tail
  in
  loop [] [] slots

let slot_of_source_id = fun snapshot source_id ->
  snapshot.analyses |> List.find_opt
    (fun (slot: analysis_slot) ->
      SourceId.equal slot.source_id source_id)

let module_slots = fun snapshot module_name ->
  Collections.HashMap.get snapshot.module_slots_by_name module_name |> Option.unwrap_or ~default:[]

let rooted_slots = fun snapshot ->
  snapshot.analyses
  |> List.filter
    (fun (slot: analysis_slot) -> snapshot.roots |> List.exists (SourceId.equal slot.source_id))

let rooted_module_names = fun snapshot -> rooted_slots snapshot |> module_names_of_slots

let loaded_ambient_cache_key = fun (snapshot: t) ~current_module_name kind ->
  digest_key [ snapshot.cache_namespace_key; kind; current_module_name ]

let loaded_ambient_env_for = fun (snapshot: t) (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  let cache_key = loaded_ambient_cache_key snapshot ~current_module_name "loaded-ambient-env" in
  match Collections.HashMap.get snapshot.shared_loaded_ambient_env_cache cache_key with
  | Some ambient -> ambient
  | None ->
      let ambient = slot.config.loaded_modules
      |> List.filter
        (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
      |> List.concat_map
        (fun typings ->
          qualify_exports
            (ModuleTypings.module_name typings)
            (ModuleTypings.type_decls typings)
            (ModuleTypings.exports typings)) in
      let _ = Collections.HashMap.insert snapshot.shared_loaded_ambient_env_cache cache_key ambient in
      ambient

let loaded_ambient_type_decls_for = fun (snapshot: t) (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  let cache_key = loaded_ambient_cache_key snapshot ~current_module_name "loaded-ambient-type-decls" in
  match Collections.HashMap.get snapshot.shared_loaded_ambient_type_decls_cache cache_key with
  | Some ambient_type_decls -> ambient_type_decls
  | None ->
      let ambient_type_decls = slot.config.loaded_modules
      |> List.filter
        (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
      |> List.concat_map
        (fun typings ->
          ModuleTypings.type_decls typings |> qualify_type_decls (ModuleTypings.module_name typings)) in
      let _ = Collections.HashMap.insert snapshot.shared_loaded_ambient_type_decls_cache cache_key ambient_type_decls in
      ambient_type_decls

let module_dependencies_of_source = fun (source: Source.t) ->
  match Syn.Deps.of_parse_result source.parse_result with
  | Ok deps -> Syn.Deps.modules deps
  | Error _ -> []

let is_alias_module_name = fun module_name -> String.ends_with ~suffix:"__Aliases" module_name

let required_local_module_names = fun (slot: analysis_slot) ->
  let current_segments = Source.module_name slot.source |> LocalModules.split_internal_module_name in
  let should_include_implicit_open module_name =
    if List.length current_segments <= 1 || not (is_alias_module_name module_name) then
      true
    else
      let alias_segments = module_name |> LocalModules.split_internal_module_name in
      match List.rev alias_segments with
      | "Aliases" :: reversed_prefix ->
          let prefix = List.rev reversed_prefix in
          not
            (List.length prefix > 0
            && List.length prefix <= List.length current_segments
            && List.for_all2 String.equal prefix (List.take (List.length prefix) current_segments))
      | _ -> true
  in
  let names = module_dependencies_of_source slot.source
  @ (slot.source.implicit_opens |> List.map IdentPath.to_string |> List.filter should_include_implicit_open) in
  names |> List.sort_uniq String.compare

let dedupe_preserving_order = fun names ->
  let seen = Collections.HashSet.with_capacity (List.length names + 1) in
  names |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let ambient_module_names_of_local_module_name = fun module_name ->
  module_name
  |> LocalModules.InternalName.of_string
  |> LocalModules.ambient_names_of_internal_name
  |> List.map LocalModules.AmbientName.to_string

let shared_cache_namespace = fun (snapshot: t) -> snapshot.cache_namespace_key

let module_result_cache_key = fun module_name -> module_name

let local_module_names_for = fun (snapshot: t) (slot: analysis_slot) ->
  match Collections.HashMap.get snapshot.local_module_names_cache (SourceId.to_int slot.source_id) with
  | Some local_module_names -> local_module_names
  | None ->
      let current_module_name = LocalModules.InternalName.of_string (Source.module_name slot.source) in
      let required_local_modules = required_local_module_names slot in
      let candidate_module_names = snapshot.module_names
      |> List.filter
        (fun candidate_module_name ->
          not
            (String.equal (LocalModules.InternalName.to_string current_module_name) candidate_module_name)) in
      let local_module_names =
        required_local_modules
        |> List.concat_map
          (fun required_module_name ->
            let required_module_name = LocalModules.RequiredName.of_string required_module_name in
            let matching_candidates = candidate_module_names
            |> List.filter_map
              (fun candidate_module_name ->
                LocalModules.contextual_match_depth
                  ~current_module_name
                  ~required_module_name
                  ~candidate_module_name:(LocalModules.InternalName.of_string candidate_module_name)
                |> Option.map (fun depth -> (candidate_module_name, depth))) in
            let best_depth = matching_candidates
            |> List.fold_left
              (fun best (_, depth) -> Some (Option.unwrap_or ~default:depth best |> Int.max depth))
              None in
            match best_depth with
            | None -> []
            | Some best_depth ->
                matching_candidates |> List.filter_map
                  (fun (candidate_module_name, depth) ->
                    if Int.equal depth best_depth then
                      Some candidate_module_name
                    else
                      None))
        |> dedupe_preserving_order
        |> List.stable_sort
          (fun left right ->
            match (is_alias_module_name left, is_alias_module_name right) with
            | (false, true) -> (-1)
            | (true, false) -> 1
            | _ -> 0)
      in
      let _ = Collections.HashMap.insert
        snapshot.local_module_names_cache
        (SourceId.to_int slot.source_id)
        local_module_names in
      local_module_names

let placeholder_analysis = fun (slot: analysis_slot) ->
  {
    SourceAnalysis.source = slot.source;
    parse_diagnostics = Syn.Parser.(slot.source.parse_result.diagnostics);
    cst = slot.source.cst;
    semantic_tree = None;
    lowering_diagnostics = [];
    typing_diagnostics = [];
    ambient_type_decls = [];
    completeness = SourceAnalysis.Partial;
    file_summary = FileSummary.partial ~source_id:slot.source_id ();
    export_bindings = [];
    type_index = Analysis.TypeIndex.empty;
    item_traces = [];
    expr_traces = [];
  }

let with_partial_module_view = fun module_typings (analysis: SourceAnalysis.t) ->
  let file_summary = ModuleTypings.to_file_summary ~source_id:analysis.source.source_id module_typings in
  {
    analysis
    with completeness = SourceAnalysis.completeness_of_file_summary file_summary;
    file_summary
  }

type partial_source_kind =
  | InterfaceSource
  | ImplementationSource

let partial_source_kind = fun (source: Source.t) ->
  match source.cst with
  | Syn.Cst.Interface _ -> InterfaceSource
  | Syn.Cst.Implementation _ -> ImplementationSource

let select_partial_source = fun sources desired_kind ->
  sources
  |> List.find_opt
    (fun (source, (_analysis: SourceAnalysis.t)) -> partial_source_kind source = desired_kind)

let placeholder_source_analysis_cache_key = fun (snapshot: t) (slot: analysis_slot) ->
  digest_key
    [
      shared_cache_namespace snapshot;
      "partial-source";
      Int.to_string (SourceId.to_int slot.source_id);
      Int.to_string slot.source.revision;
    ]

let placeholder_module_result_cache_key = fun (snapshot: t) module_name ->
  digest_key [ shared_cache_namespace snapshot; "partial-module"; module_name ]

(* These caches depend on the visible ambient module surfaces, not on the
   logical source identity that happens to request them. *)

let rec source_analysis_cache_key = fun (snapshot: t) (slot: analysis_slot) ->
  match Collections.HashMap.get snapshot.source_analysis_keys_cache (SourceId.to_int slot.source_id) with
  | Some (Some cache_key) ->
      cache_key
  | Some None ->
      placeholder_source_analysis_cache_key snapshot slot
  | None ->
      let _ = Collections.HashMap.insert
        snapshot.source_analysis_keys_cache
        (SourceId.to_int slot.source_id)
        None in
      let cache_key = digest_key
        ([
          shared_cache_namespace snapshot;
          "source";
          Int.to_string (SourceId.to_int slot.source_id);
          Int.to_string slot.source.revision;
        ]
        @ (local_module_names_for snapshot slot
        |> List.sort_uniq String.compare
        |> List.map (module_result_shared_cache_key snapshot))) in
      let _ = Collections.HashMap.insert
        snapshot.source_analysis_keys_cache
        (SourceId.to_int slot.source_id)
        (Some cache_key) in
      cache_key

and module_result_shared_cache_key = fun (snapshot: t) module_name ->
  match Collections.HashMap.get snapshot.module_result_keys_cache module_name with
  | Some (Some cache_key) ->
      cache_key
  | Some None ->
      placeholder_module_result_cache_key snapshot module_name
  | None ->
      let _ = Collections.HashMap.insert snapshot.module_result_keys_cache module_name None in
      let cache_key = digest_key
        (
          [ shared_cache_namespace snapshot; "module"; module_name; ] @ (
            module_slots snapshot module_name |> List.sort
              (fun (left: analysis_slot) right ->
                SourceId.compare left.source_id right.source_id) |> List.map
              (source_analysis_cache_key snapshot)
          )
        )
      in
      let _ = Collections.HashMap.insert
        snapshot.module_result_keys_cache
        module_name
        (Some cache_key) in
      cache_key

and local_ambient_cache_key = fun (snapshot: t) (slot: analysis_slot) ->
  match Collections.HashMap.get snapshot.local_ambient_keys_cache (SourceId.to_int slot.source_id) with
  | Some cache_key -> cache_key
  | None ->
      let cache_key = digest_key
        ([ shared_cache_namespace snapshot; "local-ambient"; ]
        @ (local_module_names_for snapshot slot
        |> List.map
          (fun module_name ->
            format
              Format.[
                str module_name;
                str ":";
                str (module_result_shared_cache_key snapshot module_name)
              ]))) in
      let _ = Collections.HashMap.insert
        snapshot.local_ambient_keys_cache
        (SourceId.to_int slot.source_id)
        cache_key in
      cache_key

and ambient_surface_cache_key = fun (snapshot: t) (slot: analysis_slot) ->
  match Collections.HashMap.get snapshot.ambient_surface_keys_cache (SourceId.to_int slot.source_id) with
  | Some cache_key -> cache_key
  | None ->
      let current_module_name = Source.module_name slot.source in
      let cache_key = digest_key
        [
          shared_cache_namespace snapshot;
          "ambient-surface";
          loaded_ambient_cache_key snapshot ~current_module_name "loaded-ambient-surface";
          local_ambient_cache_key snapshot slot;
        ] in
      let _ = Collections.HashMap.insert
        snapshot.ambient_surface_keys_cache
        (SourceId.to_int slot.source_id)
        cache_key in
      cache_key

let rec force_analysis = fun (snapshot: t) (slot: analysis_slot) ->
  match slot.state with
  | Finished analysis ->
      analysis
  | InProgress ->
      placeholder_analysis slot
  | NotStarted ->
      let analysis_cache_key = source_analysis_cache_key snapshot slot in
      (
        match Collections.HashMap.get snapshot.shared_source_analysis_cache analysis_cache_key with
        | Some analysis ->
            slot.analysis <- Some analysis;
            slot.state <- Finished analysis;
            analysis
        | None ->
            slot.state <- InProgress;
            let local_module_names = local_module_names_for snapshot slot in
            let local_ambient = local_ambient_env_for snapshot slot in
            let local_ambient_type_decls = local_ambient_type_decls_for snapshot slot in
            let ambient = slot.config.ambient @ loaded_ambient_env_for snapshot slot @ local_ambient in
            let ambient_type_decls = slot.config.ambient_type_decls
            @ loaded_ambient_type_decls_for snapshot slot
            @ local_ambient_type_decls in
            let config = slot.config
            |> TypConfig.with_ambient ~ambient
            |> TypConfig.with_ambient_type_decls ~ambient_type_decls in
            let initial_env = initial_env_for snapshot slot config in
            TypConfig.emit_event slot.config
              (fun () ->
                Event.SourceAnalysisStarted {
                  source_id = slot.source_id;
                  module_name = Source.module_name slot.source;
                  mode = Event.SnapshotAnalysis;
                  local_module_names;
                  loaded_module_count = List.length slot.config.loaded_modules;
                  ambient_binding_count = List.length ambient;
                  ambient_type_decl_count = List.length ambient_type_decls;
                });
            let analysis = SourceAnalysis.analyze ~initial_env ~config slot.source in
            TypConfig.emit_event slot.config
              (fun () ->
                Event.SourceAnalysisFinished {
                  source_id = slot.source_id;
                  module_name = Source.module_name slot.source;
                  mode = Event.SnapshotAnalysis;
                  parse_diagnostic_count = List.length analysis.parse_diagnostics;
                  lowering_diagnostic_count = List.length analysis.lowering_diagnostics;
                  typing_diagnostic_count = List.length analysis.typing_diagnostics;
                  parse_diagnostics = analysis.parse_diagnostics;
                  lowering_diagnostics = analysis.lowering_diagnostics;
                  typing_diagnostics = analysis.typing_diagnostics;
                  export_status = export_status_of_file_summary analysis.file_summary;
                  export_count = List.length (FileSummary.exports analysis.file_summary);
                  type_decl_count = List.length (FileSummary.type_decls analysis.file_summary);
                });
            let _ = Collections.HashMap.insert
              snapshot.shared_source_analysis_cache
              analysis_cache_key
              analysis in
            slot.analysis <- Some analysis;
            slot.state <- Finished analysis;
            analysis
      )

and module_results_for = fun (snapshot: t) ->
  snapshot.module_names
  |> List.map (fun module_name -> (module_name, module_result_for snapshot module_name))

and initial_env_for = fun (snapshot: t) (slot: analysis_slot) config ->
  let cache_key = ambient_surface_cache_key snapshot slot in
  match Collections.HashMap.get snapshot.shared_initial_env_cache cache_key with
  | Some initial_env -> initial_env
  | None ->
      let initial_env = Infer.initial_env_of_config ~config in
      let _ = Collections.HashMap.insert snapshot.shared_initial_env_cache cache_key initial_env in
      initial_env

and local_ambient_env_for = fun (snapshot: t) (slot: analysis_slot) ->
  let cache_key = local_ambient_cache_key snapshot slot in
  match Collections.HashMap.get snapshot.shared_local_ambient_env_cache cache_key with
  | Some ambient -> ambient
  | None ->
      let ambient =
        local_module_names_for snapshot slot
        |> List.map (fun module_name -> (module_name, module_result_for snapshot module_name))
        |> List.map
          (fun (module_name, result) ->
            let type_decls = ModuleTypings.type_decls result.ModulePairing.module_typings in
            let exports = ModuleTypings.exports result.ModulePairing.module_typings in
            ambient_module_names_of_local_module_name module_name
            |> List.map (fun alias -> qualify_exports alias type_decls exports)
            |> List.flatten)
        |> List.flatten
      in
      let _ = Collections.HashMap.insert snapshot.shared_local_ambient_env_cache cache_key ambient in
      ambient

and local_ambient_type_decls_for = fun (snapshot: t) (slot: analysis_slot) ->
  let cache_key = local_ambient_cache_key snapshot slot in
  match Collections.HashMap.get snapshot.shared_local_ambient_type_decls_cache cache_key with
  | Some ambient_type_decls -> ambient_type_decls
  | None ->
      let ambient_type_decls =
        local_module_names_for snapshot slot
        |> List.map (fun module_name -> (module_name, module_result_for snapshot module_name))
        |> List.map
          (fun (module_name, result) ->
            let type_decls = ModuleTypings.type_decls result.ModulePairing.module_typings in
            ambient_module_names_of_local_module_name module_name
            |> List.map (fun alias -> qualify_type_decls alias type_decls)
            |> List.flatten)
        |> List.flatten
      in
      let _ = Collections.HashMap.insert snapshot.shared_local_ambient_type_decls_cache cache_key ambient_type_decls in
      ambient_type_decls

and partial_module_result = fun (snapshot: t) module_name ->
  let slots = module_slots snapshot module_name in
  let available_sources = slots
  |> List.filter_map
    (fun (slot: analysis_slot) ->
      slot.analysis |> Option.map (fun analysis -> (slot.source, analysis))) in
  let preferred_source =
    match select_partial_source available_sources InterfaceSource with
    | Some source -> Some source
    | None -> select_partial_source available_sources ImplementationSource
  in
  let module_typings =
    match preferred_source with
    | Some (_source, (analysis: SourceAnalysis.t)) ->
        let exports = FileSummary.exports analysis.file_summary in
        let type_decls = FileSummary.type_decls analysis.file_summary in
        let value_definitions = SourceAnalysis.export_definitions analysis in
        let export_result =
          match analysis.file_summary.FileSummary.export_result with
          | FileSummary.TrustedExport _
          | FileSummary.ErroredExport _ -> FileSummary.ErroredExport { exports }
          | FileSummary.NoExport -> FileSummary.NoExport
        in
        ModuleTypings.partial ~module_name ~source_hash:(ModuleTypings.synthetic_source_hash
          ~module_name
          ~export_result
          ~type_decls
          ~value_definitions
          ()) ~type_decls ~value_definitions
          ?exports:(
            match export_result with
            | FileSummary.ErroredExport { exports } -> Some exports
            | FileSummary.NoExport
            | FileSummary.TrustedExport _ -> None
          )
          ()
    | None -> ModuleTypings.partial
      ~module_name
      ~source_hash:(ModuleTypings.synthetic_source_hash
        ~module_name
        ~export_result:FileSummary.NoExport
        ~type_decls:[]
        ())
      ()
  in
  let analyses_by_source =
    slots
    |> List.map
      (fun (slot: analysis_slot) ->
        let analysis =
          match slot.analysis with
          | Some analysis -> with_partial_module_view module_typings analysis
          | None -> placeholder_analysis slot
        in
        (slot.source_id, analysis))
  in
  { ModulePairing.module_typings; analyses_by_source; signature_mismatches = [] }

and module_result_for = fun (snapshot: t) module_name ->
  let cache_key = module_result_cache_key module_name in
  let shared_cache_key = module_result_shared_cache_key snapshot module_name in
  let module_is_in_progress =
    module_slots snapshot module_name
    |> List.exists
      (fun (slot: analysis_slot) ->
        match slot.state with
        | InProgress -> true
        | NotStarted
        | Finished _ -> false)
  in
  match Collections.HashMap.get snapshot.module_results_cache cache_key with
  | Some result -> result
  | None -> (
      match Collections.HashMap.get snapshot.shared_module_result_cache shared_cache_key with
      | Some result ->
          let _ = Collections.HashMap.insert snapshot.module_results_cache cache_key result in
          result
      | None ->
          if module_is_in_progress then
            partial_module_result snapshot module_name
          else
            let slots = module_slots snapshot module_name in
            let source_ids = slots |> List.map (fun (slot: analysis_slot) -> slot.source_id) in
            (
              match slots with
              | [] -> ()
              | slot :: _ -> TypConfig.emit_event
                slot.config
                (fun () -> Event.ModulePairingStarted { module_name; source_ids })
            );
            let sources = slots
            |> List.map (fun (slot: analysis_slot) -> (slot.source, force_analysis snapshot slot)) in
            let result = ModulePairing.of_sources ~module_name sources in
            let _ = Collections.HashMap.insert snapshot.module_results_cache cache_key result in
            let _ = Collections.HashMap.insert snapshot.shared_module_result_cache shared_cache_key result in
            (
              match slots with
              | [] -> ()
              | slot :: _ ->
                  TypConfig.emit_event slot.config
                    (fun () ->
                      Event.ModulePairingFinished {
                        module_name;
                        source_ids;
                        export_status = export_status_of_module_typings result.ModulePairing.module_typings;
                        export_count = List.length
                          (ModuleTypings.exports result.ModulePairing.module_typings);
                        type_decl_count = List.length
                          (ModuleTypings.type_decls result.ModulePairing.module_typings);
                        mismatch_count = List.length result.ModulePairing.signature_mismatches;
                        mismatch_subjects = result.ModulePairing.signature_mismatches
                        |> List.map signature_mismatch_subject
                        |> List.sort_uniq String.compare;
                        mismatch_messages = result.ModulePairing.signature_mismatches |> List.map signature_mismatch_message;
                      })
            );
            result
    )

let revision = fun snapshot -> snapshot.revision

let roots = fun snapshot -> snapshot.roots

let module_result_of_source = fun snapshot source_id ->
  slot_of_source_id snapshot source_id |> Option.map
    (fun (slot: analysis_slot) ->
      let module_name = Source.module_name slot.source in
      (module_name, Some (module_result_for snapshot module_name)))

let loaded_module_typings = fun snapshot ->
  match snapshot.analyses with
  | [] -> []
  | slot :: _ -> slot.config.loaded_modules

let is_root = fun snapshot source_id -> snapshot.roots |> List.exists (SourceId.equal source_id)

let analyses = fun snapshot ->
  rooted_slots snapshot |> List.filter_map
    (fun (slot: analysis_slot) ->
      match module_results_for snapshot |> List.assoc_opt (Source.module_name slot.source) with
      | Some result -> List.assoc_opt slot.source_id ModulePairing.(result.analyses_by_source)
      | None -> None)

let file_summaries = fun snapshot ->
  analyses snapshot |> List.map (fun (analysis: SourceAnalysis.t) -> analysis.file_summary)

let module_typings = fun snapshot ->
  let rooted_module_names = rooted_module_names snapshot in
  rooted_module_names
  |> List.filter_map
    (fun module_name ->
      module_results_for snapshot
      |> List.assoc_opt module_name
      |> Option.map (fun result -> ModulePairing.(result.module_typings)))

let find_module_typings = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> Some ModulePairing.(result.module_typings)
    | Some (_, None)
    | None -> None

let find_module_typings_by_name = fun snapshot module_name ->
  match module_results_for snapshot |> List.assoc_opt module_name with
  | Some result -> Some ModulePairing.(result.module_typings)
  | None ->
      loaded_module_typings snapshot |> List.find_opt
        (fun typings ->
          String.equal module_name (ModuleTypings.module_name typings))

let find_analysis = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> List.assoc_opt
      source_id
      ModulePairing.(result.analyses_by_source)
    | Some (_, None)
    | None -> None
