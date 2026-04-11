open Std
open Model

type analysis_mode =
  | BaseAnalysis
  | SnapshotAnalysis

type export_status =
  | TrustedExport
  | ErroredExport
  | MissingExport

type kind =
  | PrepareSnapshotStarted of {
      roots: SourceId.t list;
      root_modules: string list;
      session_source_count: int;
      loaded_module_count: int
    }
  | HydrateModuleTypingsStarted of { roots: SourceId.t list; missing_modules: string list }
  | HydrateModuleTypingsFinished of {
      roots: SourceId.t list;
      hydrated_modules: string list;
      loaded_module_count: int
    }
  | PrepareSnapshotFailed of {
      roots: SourceId.t list;
      missing_root_source_ids: SourceId.t list;
      missing_modules: string list
    }
  | PrepareSnapshotFinished of {
      roots: SourceId.t list;
      local_source_count: int;
      loaded_module_count: int;
      revision: int
    }
  | SnapshotMaterializationStarted of {
      roots: SourceId.t list;
      local_source_count: int;
      revision: int
    }
  | SnapshotMaterializationFinished of {
      roots: SourceId.t list;
      local_source_count: int;
      module_count: int;
      revision: int
    }
  | ModuleTypingsCollectionStarted of { roots: SourceId.t list; rooted_module_count: int }
  | ModuleTypingsCollectionFinished of {
      roots: SourceId.t list;
      rooted_module_count: int;
      produced_module_count: int
    }
  | SourceAnalysisStarted of {
      source_id: SourceId.t;
      module_name: string;
      mode: analysis_mode;
      local_module_names: string list;
      loaded_module_count: int;
      ambient_binding_count: int;
      ambient_type_decl_count: int
    }
  | SourceAnalysisFinished of {
      source_id: SourceId.t;
      module_name: string;
      mode: analysis_mode;
      parse_diagnostic_count: int;
      lowering_diagnostic_count: int;
      typing_diagnostic_count: int;
      parse_diagnostics: Syn.Diagnostic.t list;
      lowering_diagnostics: Diagnostic.t list;
      typing_diagnostics: Diagnostic.t list;
      export_status: export_status;
      export_count: int;
      type_decl_count: int
    }
  | ModulePairingStarted of { module_name: string; source_ids: SourceId.t list }
  | ModulePairingFinished of {
      module_name: string;
      source_ids: SourceId.t list;
      export_status: export_status;
      export_count: int;
      type_decl_count: int;
      mismatch_count: int;
      mismatch_subjects: string list;
      mismatch_messages: string list
    }

type t = {
  instant_us: int;
  kind: kind;
}

let source_ids_to_json = fun source_ids ->
  Data.Json.Array (source_ids
  |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id)))

let strings_to_json = fun values ->
  Data.Json.Array (values |> List.map (fun value -> Data.Json.String value))

let parse_diagnostics_to_json = fun diagnostics ->
  Data.Json.Array (List.map Syn.Diagnostic.to_json diagnostics)

let diagnostics_to_json = fun diagnostics ->
  Data.Json.Array (List.map Diagnostic.to_json diagnostics)

let analysis_mode_to_string = fun value ->
  match value with
  | BaseAnalysis -> "base"
  | SnapshotAnalysis -> "snapshot"

let export_status_to_string = fun value ->
  match value with
  | TrustedExport -> "trusted"
  | ErroredExport -> "errored"
  | MissingExport -> "missing"

let object_with_instant = fun instant_us fields ->
  Data.Json.Object (fields @ [ ("instant_us", Data.Json.Int instant_us) ])

let to_json = fun event ->
  let instant_us = event.instant_us in
  match event.kind with
  | PrepareSnapshotStarted { roots; root_modules; session_source_count; loaded_module_count } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_prepare_snapshot_start");
      ("roots", source_ids_to_json roots);
      ("root_modules", strings_to_json root_modules);
      ("session_source_count", Data.Json.Int session_source_count);
      ("loaded_module_count", Data.Json.Int loaded_module_count);
    ]
  | HydrateModuleTypingsStarted { roots; missing_modules } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_hydrate_module_typings_start");
      ("roots", source_ids_to_json roots);
      ("missing_modules", strings_to_json missing_modules);
    ]
  | HydrateModuleTypingsFinished { roots; hydrated_modules; loaded_module_count } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_hydrate_module_typings_finish");
      ("roots", source_ids_to_json roots);
      ("hydrated_modules", strings_to_json hydrated_modules);
      ("loaded_module_count", Data.Json.Int loaded_module_count);
    ]
  | PrepareSnapshotFailed { roots; missing_root_source_ids; missing_modules } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_prepare_snapshot_failed");
      ("roots", source_ids_to_json roots);
      ("missing_root_source_ids", source_ids_to_json missing_root_source_ids);
      ("missing_modules", strings_to_json missing_modules);
    ]
  | PrepareSnapshotFinished { roots; local_source_count; loaded_module_count; revision } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_prepare_snapshot_finish");
      ("roots", source_ids_to_json roots);
      ("local_source_count", Data.Json.Int local_source_count);
      ("loaded_module_count", Data.Json.Int loaded_module_count);
      ("revision", Data.Json.Int revision);
    ]
  | SnapshotMaterializationStarted { roots; local_source_count; revision } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_snapshot_materialization_start");
      ("roots", source_ids_to_json roots);
      ("local_source_count", Data.Json.Int local_source_count);
      ("revision", Data.Json.Int revision);
    ]
  | SnapshotMaterializationFinished { roots; local_source_count; module_count; revision } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_snapshot_materialization_finish");
      ("roots", source_ids_to_json roots);
      ("local_source_count", Data.Json.Int local_source_count);
      ("module_count", Data.Json.Int module_count);
      ("revision", Data.Json.Int revision);
    ]
  | ModuleTypingsCollectionStarted { roots; rooted_module_count } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_module_typings_collection_start");
      ("roots", source_ids_to_json roots);
      ("rooted_module_count", Data.Json.Int rooted_module_count);
    ]
  | ModuleTypingsCollectionFinished { roots; rooted_module_count; produced_module_count } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_module_typings_collection_finish");
      ("roots", source_ids_to_json roots);
      ("rooted_module_count", Data.Json.Int rooted_module_count);
      ("produced_module_count", Data.Json.Int produced_module_count);
    ]
  | SourceAnalysisStarted {
    source_id;
    module_name;
    mode;
    local_module_names;
    loaded_module_count;
    ambient_binding_count;
    ambient_type_decl_count
  } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_source_analysis_start");
      ("source_id", Data.Json.Int (SourceId.to_int source_id));
      ("module_name", Data.Json.String module_name);
      ("mode", Data.Json.String (analysis_mode_to_string mode));
      ("local_module_names", strings_to_json local_module_names);
      ("loaded_module_count", Data.Json.Int loaded_module_count);
      ("ambient_binding_count", Data.Json.Int ambient_binding_count);
      ("ambient_type_decl_count", Data.Json.Int ambient_type_decl_count);
    ]
  | SourceAnalysisFinished {
    source_id;
    module_name;
    mode;
    parse_diagnostic_count;
    lowering_diagnostic_count;
    typing_diagnostic_count;
    parse_diagnostics;
    lowering_diagnostics;
    typing_diagnostics;
    export_status;
    export_count;
    type_decl_count
  } ->
      object_with_instant instant_us
        [
          ("type", Data.Json.String "typ_source_analysis_finish");
          ("source_id", Data.Json.Int (SourceId.to_int source_id));
          ("module_name", Data.Json.String module_name);
          ("mode", Data.Json.String (analysis_mode_to_string mode));
          ("parse_diagnostic_count", Data.Json.Int parse_diagnostic_count);
          ("lowering_diagnostic_count", Data.Json.Int lowering_diagnostic_count);
          ("typing_diagnostic_count", Data.Json.Int typing_diagnostic_count);
          ("parse_diagnostics", parse_diagnostics_to_json parse_diagnostics);
          ("lowering_diagnostics", diagnostics_to_json lowering_diagnostics);
          ("typing_diagnostics", diagnostics_to_json typing_diagnostics);
          ("export_status", Data.Json.String (export_status_to_string export_status));
          ("export_count", Data.Json.Int export_count);
          ("type_decl_count", Data.Json.Int type_decl_count);
        ]
  | ModulePairingStarted { module_name; source_ids } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_module_pairing_start");
      ("module_name", Data.Json.String module_name);
      ("source_ids", source_ids_to_json source_ids);
    ]
  | ModulePairingFinished {
    module_name;
    source_ids;
    export_status;
    export_count;
    type_decl_count;
    mismatch_count;
    mismatch_subjects;
    mismatch_messages
  } -> object_with_instant
    instant_us
    [
      ("type", Data.Json.String "typ_module_pairing_finish");
      ("module_name", Data.Json.String module_name);
      ("source_ids", source_ids_to_json source_ids);
      ("export_status", Data.Json.String (export_status_to_string export_status));
      ("export_count", Data.Json.Int export_count);
      ("type_decl_count", Data.Json.Int type_decl_count);
      ("mismatch_count", Data.Json.Int mismatch_count);
      ("mismatch_subjects", strings_to_json mismatch_subjects);
      ("mismatch_messages", strings_to_json mismatch_messages);
    ]
