open Std

type file_result = {
  worker: Pid.t;
  result: Runner.file_result;
}

type Message.t +=
  | ScannerDiscovered of Path.t
  | ScannerComplete
  | WorkerReady of Pid.t
  | RunTask of Path.t
  | Stop
  | StopRequested
  | FileResult of file_result
  | AllComplete of Runner.summary
