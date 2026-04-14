open Std

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
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
  package_name: string;
  binary_name: string;
  source_path: Path.t;
}

type run_event =
  | Build of Riot_build.Event.t
  | RunningBinary of { package: string; binary: string; args: string list }

type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ExternalTargetLoadFailed of { target: string; reason: string }

val build_scope_for_binary:
  Riot_model.Workspace.t -> package_name:string -> binary_name:string -> Riot_build.Request.scope

val list_binaries: Riot_model.Workspace.t -> ?package_filter:string -> unit -> runnable_binary list

val run_error_message: run_error -> string

val run_event_to_json: run_event -> Data.Json.t option

val run: ?on_event:(run_event -> unit) -> run_request -> (unit, run_error) result

val run_source: ?on_event:(run_event -> unit) -> source_run_request -> (unit, run_error) result
