open Std
open Model

(** Structured debug events emitted by [typ].

    These events are intentionally coarse. They expose rooted snapshot
    preparation, module hydration, per-source analysis, and module pairing
    without forcing hosts to scrape ad hoc log output. *)
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

type t = {
  instant_us: int;
  kind: kind;
}

(** Convert a [typ] event into a machine-readable JSON object. *)
val to_json: t -> Data.Json.t
