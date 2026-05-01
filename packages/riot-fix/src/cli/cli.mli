open Std

module Types: module type of Types

module Request: module type of Request

module Catalog: module type of Catalog

module Execution: module type of Execution

type run_outcome = Types.run_outcome = {
  result: Runner.run_result;
  limit_reached: bool;
}
type event = Types.event =
  | Start of {
      mode: Runner.mode;
      concurrency: int;
    }
  | FileStarted of {
      file: Path.t;
    }
  | FileProgress of {
      file: Path.t;
      progress: Fixme.Source_runner.progress_event;
    }
  | FileResult of Runner.file_result
  | Summary of {
      summary: Runner.summary;
      limit_reached: bool;
    }

val command: ArgParser.command

val list_rules_output: format:Reporter.format -> string

val list_diagnostics_output: format:Reporter.format -> string

val run_result:
  mode:Runner.mode ->
  scope:Fix_config.scope option ->
  limit:int option ->
  files:Path.t list ->
  run_outcome

val run: ?build_package:Types.build_package -> ArgParser.matches -> (unit, exn) result

val run_args:
  ?cwd:Path.t ->
  ?on_event:(event -> unit) ->
  ?report_output:bool ->
  build_package:Types.build_package ->
  string list ->
  (unit, exn) result

val run_check_paths:
  ?cwd:Path.t ->
  ?on_event:(event -> unit) ->
  ?report_output:bool ->
  build_package:Types.build_package ->
  Path.t list ->
  (unit, exn) result

val main: ?build_package:Types.build_package -> args:string list -> unit -> (unit, exn) result
