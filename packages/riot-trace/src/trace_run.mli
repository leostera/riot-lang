open Std

type perf_options = Riot_model.Workspace_operational_config.perf_trace_policy = {
  sample_rate_hz: int option;
  call_graph: string option;
  call_graph_stack_size: int option;
}
type xctrace_options = Riot_model.Workspace_operational_config.xctrace_trace_policy = {
  template_: string option;
  time_limit: string option;
  window: string option;
}
type trace_options = {
  perf: perf_options;
  xctrace: xctrace_options;
}
type output_policy =
  | Fail_if_exists
  | Overwrite
  | Append
type trace_request = {
  output: Path.t;
  output_policy: output_policy;
  profiler: Profiler.t;
  options: trace_options;
}
type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t option;
  binary_name: string;
  profile: string;
  trace: trace_request;
  args: string list;
}
type binary_run_request = {
  binary_path: Path.t;
  binary_name: string;
  trace: trace_request;
  args: string list;
}
type event =
  | Build of Riot_build.Event.t
  | TracingBinary of {
      package: Riot_model.Package_name.t;
      binary: string;
      profiler: string;
      output: Path.t;
    }
  | TracingExternalBinary of {
      path: Path.t;
      binary: string;
      profiler: string;
      output: Path.t;
    }
type error =
  | Run of Riot_run.run_error
  | BinaryPathInvalid of {
      path: Path.t;
      reason: string;
    }
  | ProfilerUnavailable of { profiler: string; reason: string }
  | UnsupportedProfilerOption of { profiler: string; option: string; reason: string }
  | OutputAlreadyExists of Path.t
  | ProcessExited of int
  | SystemError of string

val error_message: error -> string

val event_to_json: event -> Data.Json.t option

val default_options: trace_options

val preflight: trace_request -> (unit, error) result

val run: ?on_event:(event -> unit) -> run_request -> (unit, error) result

val run_binary: ?on_event:(event -> unit) -> binary_run_request -> (unit, error) result
