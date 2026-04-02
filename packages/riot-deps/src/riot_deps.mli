open Std

module Error = Error

module Dep_solver = Dep_solver

module Lockfile_store = Lockfile_store

module Lock_refresh = Lock_refresh

module Projection = Projection

module Materializer = Materializer

module Git_dependency = Git_dependency

module Git_provenance = Git_provenance

module Publisher = Publisher

module Package_management = Package_management

type dependency_scope = Package_management.dependency_scope =
  | Runtime
  | Build
  | Dev
type manifest_selection = Package_management.manifest_selection =
  | Current
  | Workspace
  | Package of string
type suggested_package = Package_management.suggested_package = {
  package: string;
  latest_version: string;
  description: string option;
}
type search_request = Package_management.search_request = {
  query: string;
  limit: int;
}
type package_event = Package_management.event =
  | RegistryPackageLookupStarted of { package: string }
  | RegistryPackageLookupFinished of { package: string; latest_version: string }
  | SourceDependencyMaterializationStarted of { source_locator: string; ref_: string option }
  | SourceDependencyMaterializationFinished of {
      source_locator: string;
      ref_: string option;
      package: string;
      version: string option;
    }
  | PackageUpdated of { package: string; from_version: string; to_version: string }
  | ManifestUpdated of { path: Path.t; section: string; operation:
        [
          `Add
          | `Remove
        ]; dependency: string }
  | Pm of Riot_model.Event.kind
type add_request = Package_management.add_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}
type remove_request = Package_management.remove_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}
type package_error = Package_management.error =
  | CurrentPackageNotFound of { cwd: Path.t }
  | PackageNotFound of { package: string }
  | DependencySpecInvalid of { dependency: string; error: string }
  | PathDependencyMustBeRelative of { dependency: string }
  | PathDependencyLoadFailed of { dependency: string; path: Path.t; error: string }
  | SourceDependencyLoadFailed of {
      dependency: string;
      source_locator: string;
      ref_: string option;
      error: string;
    }
  | RegistryInitializationFailed of { registry: string; error: string }
  | RegistryLookupFailed of { package: string; registry: string; error: string }
  | RegistrySearchFailed of { query: string; registry: string; error: string }
  | RegistryPackageNotFound of { package: string; registry: string; suggestions: suggested_package list }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of { path: Path.t; error: string }
  | DependencyNotFoundInSection of { path: Path.t; section: string; dependency: string }
  | WorkspaceReloadFailed of { workspace_root: Path.t; error: string }
  | WorkspaceReloadHadErrors of { workspace_root: Path.t; errors: string list }
  | LockRefreshFailed of Error.t
val package_error_message: package_error -> string

type event_sink = Riot_model.Event.kind -> unit
val ensure_lock:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  ((Riot_model.Lockfile.t * Riot_model.Package.resolved list), Error.t) result

val ensure_workspace:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  (Riot_model.Workspace.t, Error.t) result

val add:
  ?on_event:(package_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  cwd:Path.t ->
  request:add_request ->
  unit ->
  (unit, package_error) result

val search:
  ?registry:Pkgs_ml.Registry.t ->
  request:search_request ->
  unit ->
  (suggested_package list, package_error) result

val remove:
  ?on_event:(package_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  cwd:Path.t ->
  request:remove_request ->
  unit ->
  (unit, package_error) result

val update:
  ?on_event:(package_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  (unit, package_error) result
