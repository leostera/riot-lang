open Std

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  binary_name: string;
  args: string list;
}
type source_run_request = {
  source_spec: string;
  binary_name: string;
  update: bool;
  args: string list;
}
type run_event =
  | Build of Build_runtime.build_event
  | RunningBinary of { package: string; binary: string; args: string list }
type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Build_runtime.build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error
val build_scope_for_binary:
  Riot_model.Workspace.t -> package_name:string -> binary_name:string -> Build_runtime.build_scope

val run_error_message: run_error -> string

val run_event_to_json: run_event -> Data.Json.t option

val run: ?on_event:(run_event -> unit) -> run_request -> (unit, run_error) result

val run_source: ?on_event:(run_event -> unit) -> source_run_request -> (unit, run_error) result
