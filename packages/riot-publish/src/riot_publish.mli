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
  | Fix of Riot_fix.Event.t
  | Build of Riot_build.Event.t
  | CheckStarted of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | CheckFinished of { package: string; version: Std.Version.t option; stage: publish_check_stage }
  | Packing of { package: string; version: Std.Version.t; artifact_path: Path.t }
  | SkippedNotPublic of { package: string; version: Std.Version.t option }
  | SkippedAlreadyPublished of { package: string; version: Std.Version.t }
  | DryRunPlanned of Riot_deps.Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release
type publish_outcome =
  | SkippedNotPublicPackage of { package: string; version: Std.Version.t option }
  | Skipped of { package: string; version: Std.Version.t }
  | Planned of Riot_deps.Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release
type publish_error =
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Riot_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspaceScanFailed of { workspace_root: Path.t; error: string }
  | FmtCheckFailed of { package: string; error: string }
  | FixCheckFailed of { package: string; error: string }
  | BuildCheckFailed of { package: string; error: string }
  | PublishPlanFailed of Riot_deps.Publisher.error
  | PublishFailed of { package: string; error: Riot_deps.Publisher.error }
val publish_error_message: publish_error -> string

module For_test : sig
  type deps = {
    resolve_registry: unit -> (Pkgs_ml.Registry.t, publish_error) result;
    load_api_token: registry_name:string -> (string, publish_error) result;
    workspace_publish_order:
      packages:Riot_model.Package.t list ->
      (Riot_model.Package.t list, publish_error) result;
    published_version_exists:
      registry:Pkgs_ml.Registry.t ->
      package_name:string ->
      version:Std.Version.t ->
      (bool, publish_error) result;
    run_fmt_check:
      emit:(publish_event -> unit) ->
      workspace:Riot_model.Workspace.t ->
      package:Riot_model.Package.t ->
      (unit, publish_error) result;
    run_fix_check:
      emit:(publish_event -> unit) ->
      registry:Pkgs_ml.Registry.t ->
      workspace:Riot_model.Workspace.t ->
      request:publish_request ->
      package:Riot_model.Package.t ->
      (unit, publish_error) result;
    run_build_check:
      emit:(publish_event -> unit) ->
      workspace:Riot_model.Workspace.t ->
      package_name:string ->
      profile:string ->
      (unit, publish_error) result;
    plan_publish:
      registry:Pkgs_ml.Registry.t ->
      publishing_workspace_packages:string list ->
      package:Riot_model.Package.t ->
      (Riot_deps.Publisher.publish_plan, publish_error) result;
    prepare_publish_artifact:
      target_dir_root:Path.t ->
      Riot_deps.Publisher.publish_plan ->
      (Riot_deps.Publisher.prepared_publish, publish_error) result;
    publish_prepared:
      registry:Pkgs_ml.Registry.t ->
      api_token:string ->
      Riot_deps.Publisher.prepared_publish ->
      (Pkgs_ml.Registry.published_release, publish_error) result;
  }

  val default_deps: deps

  val publish_with:
    ?on_event:(publish_event -> unit) ->
    deps:deps ->
    workspace:Riot_model.Workspace.t ->
    request:publish_request ->
    mode:publish_mode ->
    unit ->
    (publish_outcome list, publish_error) result
end

val publish:
  ?on_event:(publish_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  request:publish_request ->
  mode:publish_mode ->
  unit ->
  (publish_outcome list, publish_error) result
