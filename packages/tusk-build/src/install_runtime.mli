open Std

type install_request = {
  workspace: Tusk_model.Workspace.t;
  binary_name: string;
  local_only: bool;
}
type install_event =
  | Build of Build_runtime.build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | PromotionWarning of { binary: string; destination: Path.t; global: bool; reason: string }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }
type install_error =
  | BinaryNotFound of { binary_name: string }
  | BuildFailed of Build_runtime.build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ClientError of Client.error
val install_error_message: install_error -> string

val install_event_to_json: install_event -> Data.Json.t option

val install: ?on_event:(install_event -> unit) -> install_request -> (unit, install_error) result
