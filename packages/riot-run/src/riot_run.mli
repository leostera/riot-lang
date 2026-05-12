open Std

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t option;
  binary_name: string;
  profile: string;
  args: string list;
}
type source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  update: bool;
  args: string list;
}
type runnable_binary = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  source_path: Path.t;
}
type built_binary = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  path: Path.t;
  args: string list;
}
type running_binary
type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
    }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
      reason: string;
    }
  | ProcessExited of int
  | SystemError of string
  | ExternalTargetLoadFailed of {
      target: string;
      error: Riot_deps.package_error;
    }

val build_scope_for_binary:
  Riot_model.Workspace.t ->
  package_name:Riot_model.Package_name.t ->
  binary_name:string ->
  Riot_build.Request.scope

val list_binaries:
  Riot_model.Workspace.t ->
  ?package_filter:Riot_model.Package_name.t ->
  unit ->
  runnable_binary list

val resolve_binary:
  workspace:Riot_model.Workspace.t ->
  package_name:Riot_model.Package_name.t option ->
  binary_name:string ->
  (Riot_model.Package_name.t, run_error) result

val run_error_message: run_error -> string

val build_binary:
  ?on_event:(Riot_model.Event.t -> unit) ->
  run_request ->
  (built_binary, run_error) result

val build_source_binary:
  ?on_event:(Riot_model.Event.t -> unit) ->
  source_run_request ->
  (built_binary, run_error) result

val start_built_binary:
  ?on_event:(Riot_model.Event.t -> unit) ->
  built_binary ->
  (running_binary, run_error) result

val try_wait_running_binary: running_binary -> ((unit, run_error) result option, run_error) result

val wait_running_binary: running_binary -> (unit, run_error) result

val terminate_running_binary: running_binary -> (unit, run_error) result

val run: ?on_event:(Riot_model.Event.t -> unit) -> run_request -> (unit, run_error) result

val run_source:
  ?on_event:(Riot_model.Event.t -> unit) ->
  source_run_request ->
  (unit, run_error) result
