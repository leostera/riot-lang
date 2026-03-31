open Std

type mode =
  | Check
  | Apply
type file_result = {
  file: Path.t;
  final_source: string;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
  applied_fixes: Fix.fix list;
  changed: bool;
  error: string option;
}
type summary = {
  total_files: int;
  changed_files: int;
  remaining_diagnostics: int;
  applied_fixes: int;
  failed_files: int;
}
type run_result = {
  files: file_result list;
  summary: summary;
}
val run_file:
  ?pipeline:Pipeline.t -> ?pipeline_for_file:(Path.t -> Pipeline.t) -> mode:mode -> Path.t -> file_result

val run_files:
  ?pipeline:Pipeline.t -> ?pipeline_for_file:(Path.t -> Pipeline.t) -> mode:mode -> Path.t list -> run_result

val summarize: file_result list -> summary

val summary_to_json: summary -> Data.Json.t

val file_result_to_json: file_result -> Data.Json.t

val run_result_to_json: run_result -> Data.Json.t
