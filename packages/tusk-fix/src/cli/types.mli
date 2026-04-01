open Std

type build_package = workspace_root:Path.t -> package_name:string -> profile:string -> (unit, exn) result
type run_outcome = {
  result: Runner.run_result;
  limit_reached: bool;
}
type event = Event.t =
  | Start of { mode: Runner.mode; concurrency: int }
  | FileStarted of { file: Path.t }
  | FileProgress of { file: Path.t; progress: Fixme.Source_runner.progress_event }
  | FileResult of Runner.file_result
  | Summary of { summary: Runner.summary; limit_reached: bool }
type output_mode =
  | Silent
  | Report of Reporter.format
val no_event: event -> unit
