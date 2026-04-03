open Std

type dependency_scope =
  | Runtime
  | Build
  | Dev
type manifest_selection =
  | Current
  | Workspace
  | Package of string
type suggested_package = {
  package: string;
  latest_version: string;
  description: string option;
}
type search_request = {
  query: string;
  limit: int;
}
type event_sink = Riot_model.Event.kind -> unit
type add_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}
type remove_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}
type error =
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
val error_message: error -> string

val add:
  ?on_event:event_sink ->
  workspace:Riot_model.Workspace.t ->
  cwd:Path.t ->
  request:add_request ->
  unit ->
  (unit, error) result

val remove:
  ?on_event:event_sink ->
  workspace:Riot_model.Workspace.t ->
  cwd:Path.t ->
  request:remove_request ->
  unit ->
  (unit, error) result

val search:
  ?registry:Pkgs_ml.Registry.t -> request:search_request -> unit -> (suggested_package list, error) result

val update: ?on_event:event_sink -> workspace:Riot_model.Workspace.t -> unit -> (unit, error) result
