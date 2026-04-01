open Std

type request =
  | Workspace
  | Package of string
type mode =
  | Dry_run
  | Publish
type check_stage =
[
  | `Fmt
  | `Fix
  | `Build
  | `Metadata
]
type event =
  | Pm of Tusk_model.Event.kind
  | Fmt of Krasny.Report.event
  | Fix of Tusk_fix.Event.t
  | CheckStarted of { package: string; stage: check_stage }
  | CheckFinished of { package: string; stage: check_stage }
  | DryRunPlanned of Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release
type outcome =
  | DryRun of Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release
type error =
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Tusk_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspacePreparationFailed of { error: Tusk_model.Pm_error.t }
  | WorkspaceScanFailed of { workspace_root: Path.t; error: string }
  | ToolchainInitializationFailed of { error: string }
  | FmtCheckFailed of { package: string; error: string }
  | FixCheckFailed of { package: string; error: string }
  | BuildCheckFailed of { package: string; error: string }
  | PublishPlanFailed of Publisher.error
  | PublishFailed of { package: string; error: Publisher.error }
val message: error -> string

val run:
  ?on_event:(event -> unit) ->
  workspace:Tusk_model.Workspace.t ->
  request:request ->
  mode:mode ->
  unit ->
  (outcome list, error) result
