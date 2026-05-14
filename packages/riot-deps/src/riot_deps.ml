open Std

module Error = Error
module Dep_solver = Dep_solver
module Lockfile_store = Lockfile_store
module Lock_refresh = Lock_refresh
module Projection = Projection
module Materializer = Materializer
module Git_dependency = Git_dependency
module Registry_package_spec = Registry_package_spec
module Git_provenance = Git_provenance
module Publisher = Publisher
module Workspace_resolution = Workspace_resolution
module Manifest_edit = Manifest_edit
module Package_management = Package_management

type event_sink = Workspace_resolution.event_sink

let ensure_lock = fun ?emit ~workspace_manager ~mode ~registry ~workspace () ->
  Workspace_resolution.ensure_lock
    ?emit
    ~workspace_manager
    ~mode
    ~registry
    ~workspace
    ()

let ensure_workspace = Workspace_resolution.ensure_workspace

let ensure_locked_dependencies = Workspace_resolution.ensure_locked_dependencies

type dependency_scope = Package_management.dependency_scope =
  | Runtime
  | Build
  | Dev

type manifest_selection = Package_management.manifest_selection =
  | Current
  | Workspace
  | Package of Riot_model.Package_name.t

type suggested_package = Package_management.suggested_package = {
  package: string;
  latest_version: string;
  description: string option;
}

type search_request = Package_management.search_request = { query: string; limit: int }

type loaded_workspace = Package_management.loaded_workspace = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t;
}

type add_request = Package_management.add_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependencies: string list;
}

type remove_request = Package_management.remove_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependencies: Riot_model.Package_name.t list;
}

type update_request = Package_management.update_request = {
  packages: Riot_model.Package_name.t list;
}

type dependency_spec_error = Package_management.dependency_spec_error =
  | RegistryDependencySpecError of Registry_package_spec.error
  | SourceDependencySpecError of Git_dependency.error

type path_dependency_load_error = Package_management.path_dependency_load_error =
  | PathDependencyManifestReadFailed of IO.error
  | PathDependencyTomlParseFailed of Std.Data.Toml.error
  | PathDependencyManifestDecodeFailed of Riot_model.Package.manifest_error

type source_dependency_load_error = Package_management.source_dependency_load_error =
  | SourceDependencyMaterializationFailed of Git_dependency.error
  | SourceDependencyManifestReadFailed of IO.error
  | SourceDependencyTomlParseFailed of Std.Data.Toml.error
  | SourceDependencyManifestDecodeFailed of Riot_model.Package.manifest_error

type registry_initialization_error = Package_management.registry_initialization_error =
  | RegistryFilesystemInitializationFailed of Pkgs_ml.Registry_cache.create_error

type registry_lookup_error = Package_management.registry_lookup_error =
  | RegistryPackageDocumentReadFailed of string
  | RegistryPackageNameDecodeFailed of Riot_model.Package_name.error

type registry_search_error = Package_management.registry_search_error =
  | RegistrySearchRequestFailed of string

type registry_materialization_error = Package_management.registry_materialization_error =
  | RegistryPackageMaterializationFailed of Error.t
  | RegistryPackageManifestReadFailed of IO.error
  | RegistryPackageTomlParseFailed of Std.Data.Toml.error
  | RegistryPackageManifestDecodeFailed of Riot_model.Package.manifest_error

type package_error = Package_management.error =
  | CurrentPackageNotFound of {
      cwd: Path.t;
    }
  | PackageNotFound of {
      package: Riot_model.Package_name.t;
    }
  | DependencySpecInvalid of {
      dependency: string;
      error: dependency_spec_error;
    }
  | PathDependencyMustBeRelative of { dependency: string }
  | PathDependencyLoadFailed of {
      dependency: string;
      path: Path.t;
      error: path_dependency_load_error;
    }
  | SourceDependencyLoadFailed of {
      dependency: string;
      source_locator: string;
      ref_: string option;
      error: source_dependency_load_error;
    }
  | RegistryInitializationFailed of {
      registry: string;
      error: registry_initialization_error;
    }
  | RegistryLookupFailed of {
      package: string;
      registry: string;
      error: registry_lookup_error;
    }
  | RegistryMaterializationFailed of {
      package: string;
      version: string;
      registry: string;
      error: registry_materialization_error;
    }
  | RegistrySearchFailed of {
      query: string;
      registry: string;
      error: registry_search_error;
    }
  | RegistryPackageNotFound of {
      package: string;
      registry: string;
      suggestions: suggested_package list;
    }
  | RegistryReleaseYanked of { package: string; version: string; registry: string }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of Manifest_edit.error
  | DependencyNotFoundInSection of {
      path: Path.t;
      section: string;
      dependency: string;
    }
  | WorkspaceReloadFailed of {
      workspace_root: Path.t;
      error: Riot_model.Workspace_manager.scan_error;
    }
  | WorkspaceReloadHadErrors of {
      workspace_root: Path.t;
      errors: Riot_model.Workspace_manager.load_error list;
    }
  | MaterializedPackageNotFound of {
      package_root: Path.t;
      workspace_root: Path.t;
    }
  | LockRefreshFailed of Error.t

let package_error_message = Package_management.error_message

let load_source_workspace = Package_management.load_source_workspace

let load_source_workspace_from_spec = Package_management.load_source_workspace_from_spec

let load_registry_workspace = Package_management.load_registry_workspace

let load_registry_workspace_from_spec = Package_management.load_registry_workspace_from_spec

let add = Package_management.add

let search = Package_management.search

let remove = Package_management.remove

let update = Package_management.update
