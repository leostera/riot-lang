open Std
open Model

type analysis_mode =
  | BaseAnalysis
  | SnapshotAnalysis

type export_status =
  | TrustedExport
  | ErroredExport
  | MissingExport

type t =
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
  | SourceAnalysisStarted of {
      source_id: SourceId.t;
      module_name: string;
      mode: analysis_mode;
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

let source_ids_to_json = fun source_ids ->
  Data.Json.Array (source_ids
  |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id)))

let strings_to_json = fun values ->
  Data.Json.Array (values |> List.map (fun value -> Data.Json.String value))

let analysis_mode_to_string = function
  | BaseAnalysis -> "base"
  | SnapshotAnalysis -> "snapshot"

let export_status_to_string = function
  | TrustedExport -> "trusted"
  | ErroredExport -> "errored"
  | MissingExport -> "missing"

let to_json = function
  | PrepareSnapshotStarted { roots; root_modules; session_source_count; loaded_module_count } -> Data.Json.Object [
    ("type", Data.Json.String "typ_prepare_snapshot_start");
    ("roots", source_ids_to_json roots);
    ("root_modules", strings_to_json root_modules);
    ("session_source_count", Data.Json.Int session_source_count);
    ("loaded_module_count", Data.Json.Int loaded_module_count);
  ]
  | HydrateModuleTypingsStarted { roots; missing_modules } -> Data.Json.Object [
    ("type", Data.Json.String "typ_hydrate_module_typings_start");
    ("roots", source_ids_to_json roots);
    ("missing_modules", strings_to_json missing_modules);
  ]
  | HydrateModuleTypingsFinished { roots; hydrated_modules; loaded_module_count } -> Data.Json.Object [
    ("type", Data.Json.String "typ_hydrate_module_typings_finish");
    ("roots", source_ids_to_json roots);
    ("hydrated_modules", strings_to_json hydrated_modules);
    ("loaded_module_count", Data.Json.Int loaded_module_count);
  ]
  | PrepareSnapshotFailed { roots; missing_root_source_ids; missing_modules } -> Data.Json.Object [
    ("type", Data.Json.String "typ_prepare_snapshot_failed");
    ("roots", source_ids_to_json roots);
    ("missing_root_source_ids", source_ids_to_json missing_root_source_ids);
    ("missing_modules", strings_to_json missing_modules);
  ]
  | PrepareSnapshotFinished { roots; local_source_count; loaded_module_count; revision } -> Data.Json.Object [
    ("type", Data.Json.String "typ_prepare_snapshot_finish");
    ("roots", source_ids_to_json roots);
    ("local_source_count", Data.Json.Int local_source_count);
    ("loaded_module_count", Data.Json.Int loaded_module_count);
    ("revision", Data.Json.Int revision);
  ]
  | SourceAnalysisStarted {
    source_id;
    module_name;
    mode;
    loaded_module_count;
    ambient_binding_count;
    ambient_type_decl_count
  } -> Data.Json.Object [
    ("type", Data.Json.String "typ_source_analysis_start");
    ("source_id", Data.Json.Int (SourceId.to_int source_id));
    ("module_name", Data.Json.String module_name);
    ("mode", Data.Json.String (analysis_mode_to_string mode));
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
    export_status;
    export_count;
    type_decl_count
  } ->
      Data.Json.Object [
        ("type", Data.Json.String "typ_source_analysis_finish");
        ("source_id", Data.Json.Int (SourceId.to_int source_id));
        ("module_name", Data.Json.String module_name);
        ("mode", Data.Json.String (analysis_mode_to_string mode));
        ("parse_diagnostic_count", Data.Json.Int parse_diagnostic_count);
        ("lowering_diagnostic_count", Data.Json.Int lowering_diagnostic_count);
        ("typing_diagnostic_count", Data.Json.Int typing_diagnostic_count);
        ("export_status", Data.Json.String (export_status_to_string export_status));
        ("export_count", Data.Json.Int export_count);
        ("type_decl_count", Data.Json.Int type_decl_count);
      ]
  | ModulePairingStarted { module_name; source_ids } -> Data.Json.Object [
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
  } -> Data.Json.Object [
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
