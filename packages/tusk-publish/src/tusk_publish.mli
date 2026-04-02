open Std

type publish_selection =
  | Workspace
  | Package of string
type publish_request = {
  selection: publish_selection;
  skip_check: bool;
}
type publish_mode =
  | DryRun
  | Publish
type publish_check_stage =
[
  | `fmt
  | `fix
  | `build
  | `metadata
]
type publish_event =
  | Fmt of Krasny.Report.event
  | Fix of Tusk_fix.Event.t
  | Build of Tusk_build.build_event
  | CheckStarted of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | CheckFinished of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | Packing of { package: string; version: Std.Version.t; artifact_path: Path.t }
  | SkippedAlreadyPublished of { package: string; version: Std.Version.t }
  | DryRunPlanned of Tusk_deps.Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release
type publish_outcome =
  | Skipped of { package: string; version: Std.Version.t }
  | Planned of Tusk_deps.Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release
type publish_error =
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Tusk_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspaceScanFailed of { workspace_root: Path.t; error: string }
  | FmtCheckFailed of { package: string; error: string }
  | FixCheckFailed of { package: string; error: string }
  | BuildCheckFailed of { package: string; error: string }
  | PublishPlanFailed of Tusk_deps.Publisher.error
  | PublishFailed of { package: string; error: Tusk_deps.Publisher.error }
val publish_error_message: publish_error -> string

val publish:
  ?on_event:(publish_event -> unit) ->
  workspace:Tusk_model.Workspace.t ->
  request:publish_request ->
  mode:publish_mode ->
  unit ->
  (publish_outcome list, publish_error) result
