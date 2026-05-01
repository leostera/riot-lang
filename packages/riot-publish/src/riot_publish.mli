open Std

type publish_selection =
  | Workspace
  | Package of Riot_model.Package_name.t
type publish_request = {
  selection: publish_selection;
  skip_check: bool;
}
type publish_mode =
  | DryRun
  | Publish
type publish_check_stage = [ | `availability | `fmt | `fix | `build | `metadata]
type publish_event =
  | Fmt of Riot_fmt.event
  | Fix of Riot_fix.Event.t
  | Build of Riot_build.Event.t
  | CheckStarted of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t option;
      stage: publish_check_stage;
    }
  | CheckFinished of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t option;
      stage: publish_check_stage;
    }
  | Packing of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t;
      artifact_path: Path.t;
    }
  | SkippedNotPublic of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t option;
    }
  | SkippedAlreadyPublished of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t;
    }
  | DryRunPlanned of Riot_deps.Publisher.prepared_publish
  | PackagePublished of Pkgs_ml.Registry.published_release
type publish_outcome =
  | SkippedNotPublicPackage of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t option;
    }
  | Skipped of {
      package: Riot_model.Package_name.t;
      version: Std.Version.t;
    }
  | Planned of Riot_deps.Publisher.prepared_publish
  | Published of Pkgs_ml.Registry.published_release
type publish_error =
  | PackageNotFound of {
      package: Riot_model.Package_name.t;
    }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Riot_model.User_config.error
  | MissingApiToken of {
      registry_name: string;
      path: Path.t;
    }
  | RegistryInitializationFailed of {
      registry_name: string;
      error: Riot_deps.registry_initialization_error;
    }
  | WorkspaceScanFailed of {
      workspace_root: Path.t;
      error: Riot_model.Workspace_manager.scan_error;
    }
  | WorkspaceLoadHadErrors of {
      workspace_root: Path.t;
      errors: Riot_model.Workspace_manager.load_error list;
    }
  | WorkspacePrepareFailed of {
      workspace_root: Path.t;
      error: Riot_model.Pm_error.t;
    }
  | FmtCheckFailed of {
      package: Riot_model.Package_name.t;
      error: exn;
    }
  | FixCheckFailed of {
      package: Riot_model.Package_name.t;
      error: exn;
    }
  | BuildCheckFailed of {
      package: Riot_model.Package_name.t;
      error: Riot_build.error;
    }
  | PublishPlanFailed of Riot_deps.Publisher.error
  | PublishFailed of {
      package: Riot_model.Package_name.t;
      error: Riot_deps.Publisher.error;
    }
val publish_error_message: publish_error -> string

module For_test: sig
  type deps = {
    resolve_registry: unit -> (Pkgs_ml.Registry.t, publish_error) result;
    load_api_token: registry_name:string -> (string, publish_error) result;
    workspace_publish_order:
      packages:Riot_model.Package.t list ->
      (Riot_model.Package.t list, publish_error) result;
    published_version_exists:
      registry:Pkgs_ml.Registry.t ->
      package_name:Riot_model.Package_name.t ->
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
      package_name:Riot_model.Package_name.t ->
      profile:string ->
      (unit, publish_error) result;
    plan_publish:
      registry:Pkgs_ml.Registry.t ->
      publishing_workspace_packages:Riot_model.Package_name.t list ->
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
