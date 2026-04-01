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
  | Fix of Tusk_fix.Event.t
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
val publish_error_message: publish_error -> string

val publish:
  ?on_event:(publish_event -> unit) ->
  workspace:Tusk_model.Workspace.t ->
  request:publish_request ->
  mode:publish_mode ->
  unit ->
  (publish_outcome list, publish_error) result

type event_sink = Tusk_model.Event.kind -> unit
val ensure_lock:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  packages:Tusk_model.Package.t list ->
  unit ->
  ((Tusk_model.Lockfile.t * Tusk_model.Package.resolved list), Error.t) result

val ensure_workspace:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Tusk_model.Workspace.t ->
  unit ->
  (Tusk_model.Workspace.t, Error.t) result
