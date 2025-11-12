open Std

type lint_result = {
  file : Path.t;
  diagnostics : Diagnostic.t list;
  source : string;
}

type worker_failure = { file : Path.t; worker : Pid.t; reason : string }

type completion_result = {
  total_files : int;
  total_diagnostics : int;
  failed_files : int;
}

type Message.t +=
  | WorkerReady of Pid.t
  | LintTask of Path.t
  | LintResult of lint_result
  | WorkerFailed of worker_failure
  | AllComplete of completion_result
