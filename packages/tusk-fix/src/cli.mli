open Std

type run_outcome = {
  result: Runner.run_result;
  limit_reached: bool;
}
type event =
  | Start of { mode: Runner.mode; concurrency: int }
  | FileStarted of { file: Path.t }
  | FileProgress of { file: Path.t; progress: Fixme.Source_runner.progress_event }
  | FileResult of Runner.file_result
  | Summary of { summary: Runner.summary; limit_reached: bool }
val command: ArgParser.command

val list_rules_output: format:Reporter.format -> string

val list_diagnostics_output: format:Reporter.format -> string

val run_result:
  mode:Runner.mode -> scope:Fix_config.scope option -> limit:int option -> files:Path.t list -> run_outcome

val run_args:
  ?cwd:Path.t ->
  ?on_event:(event -> unit) ->
  ?report_output:bool ->
  build_package:(workspace_root:Path.t -> package_name:string -> (unit, exn) result) ->
  string list ->
  (unit, exn) result

val run_check_paths:
  ?cwd:Path.t ->
  ?on_event:(event -> unit) ->
  ?report_output:bool ->
  build_package:(workspace_root:Path.t -> package_name:string -> (unit, exn) result) ->
  Path.t list ->
  (unit, exn) result

val run: ArgParser.matches -> (unit, exn) result

val main: args:string list -> (unit, exn) result
