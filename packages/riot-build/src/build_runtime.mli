open Std

type build_scope =
  | Runtime
  | Dev
type target_request =
  | Host
  | All
  | Pattern of string
type build_request = {
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}
type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event
type build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error
val error_message: build_error -> string

val build: ?on_event:(build_event -> unit) -> build_request -> (unit, build_error) result
