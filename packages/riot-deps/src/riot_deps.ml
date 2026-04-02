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
module Workspace_resolution = Workspace_resolution
module Package_management = Package_management

type event_sink = Workspace_resolution.event_sink

let ensure_lock = Workspace_resolution.ensure_lock

let ensure_workspace = Workspace_resolution.ensure_workspace

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
      version: string option
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
      error: string
    }
  | RegistryInitializationFailed of { registry: string; error: string }
  | RegistryLookupFailed of { package: string; registry: string; error: string }
  | RegistrySearchFailed of { query: string; registry: string; error: string }
  | RegistryPackageNotFound of {
      package: string;
      registry: string;
      suggestions: suggested_package list
    }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of { path: Path.t; error: string }
  | DependencyNotFoundInSection of { path: Path.t; section: string; dependency: string }
  | WorkspaceReloadFailed of { workspace_root: Path.t; error: string }
  | WorkspaceReloadHadErrors of { workspace_root: Path.t; errors: string list }
  | LockRefreshFailed of Error.t

let package_error_message = Package_management.error_message

let add = Package_management.add

let search = Package_management.search

let remove = Package_management.remove

let update = Package_management.update
