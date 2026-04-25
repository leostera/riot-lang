(** Shared message types for coordinator and worker communication *)
open Std

type file_result = { worker: Pid.t; result: Runner.file_result }

type file_progress = { worker: Pid.t; file: Path.t; event: Fixme.Source_runner.progress_event }

type Message.t +=
  | ScannerDiscovered of Path.t
  | ScannerComplete
  | FileStarted of Path.t
  | FileProgress of file_progress
  | WorkerReady of Pid.t
  | RunTask of Path.t
  | Stop
  | StopRequested
  | FileResult of file_result
  | AllComplete of Runner.summary
