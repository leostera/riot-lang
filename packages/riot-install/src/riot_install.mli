open Std

type install_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  binary_name: string;
  local_only: bool;
  promote_to_workspace_root: bool;
}

type source_install_request = {
  source_spec: string;
  binary_name: string;
  update: bool;
  local_only: bool;
}

type registry_install_request = {
  package_spec: string;
  binary_name: string;
  local_only: bool;
}

type install_event =
  | Build of Riot_build.Event.t
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }

type install_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | PromotionFailed of { binary_name: string; destination: Path.t; global: bool; reason: string }
  | ExternalTargetLoadFailed of { target: string; reason: string }

val install_error_message: install_error -> string

val install_event_to_json: install_event -> Data.Json.t option

val install: ?on_event:(install_event -> unit) -> install_request -> (unit, install_error) result

val install_source:
  ?on_event:(install_event -> unit) -> source_install_request -> (unit, install_error) result

val install_registry:
  ?on_event:(install_event -> unit) -> registry_install_request -> (unit, install_error) result
