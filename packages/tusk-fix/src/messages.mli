(** Shared message types for coordinator and worker communication *)

open Std

type file_result = {
  worker : Pid.t;
  result : Runner.file_result;
}

type Message.t +=
  | WorkerReady of Pid.t
  | RunTask of Path.t
  | Stop
  | StopRequested
  | FileResult of file_result
  | AllComplete of Runner.summary
