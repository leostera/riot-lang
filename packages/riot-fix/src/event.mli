open Std

type t =
  | Start of { mode: Runner.mode; concurrency: int }
  | FileStarted of { file: Path.t }
  | FileProgress of { file: Path.t; progress: Fixme.Source_runner.progress_event }
  | FileResult of Runner.file_result
  | Summary of { summary: Runner.summary; limit_reached: bool }
val to_json: t -> Data.Json.t
