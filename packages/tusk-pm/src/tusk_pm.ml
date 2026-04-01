open Std
module Error = Error
module Dep_solver = Dep_solver
module Lockfile_store = Lockfile_store
module Lock_refresh = Lock_refresh
module Projection = Projection
module Materializer = Materializer
module Git_provenance = Git_provenance
module Publisher = Publisher
module Publish = Publish
module Workspace_resolution = Workspace_resolution

type publish_request = Publish.request =
  | Workspace
  | Package of string

type publish_mode = Publish.mode =
  | Dry_run
  | Publish

type publish_check_stage = Publish.check_stage

type publish_event = Publish.event =
  | Pm of Tusk_model.Event.kind
  | Fmt of Krasny.Report.event
  | Fix of Tusk_fix.Cli.event
  | CheckStarted of { package: string; stage: publish_check_stage }
  | CheckFinished of { package: string; stage: publish_check_stage }
  | DryRunPlanned of Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release

type publish_outcome = Publish.outcome =
  | DryRun of Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release

type publish_error = Publish.error =
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

type event_sink = Workspace_resolution.event_sink

let publish_error_message = Publish.message

let publish = Publish.run

let ensure_lock = Workspace_resolution.ensure_lock

let ensure_workspace = Workspace_resolution.ensure_workspace
