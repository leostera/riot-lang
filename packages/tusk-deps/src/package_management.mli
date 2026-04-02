open Std

type dependency_scope =
  | Runtime
  | Build
  | Dev
type manifest_selection =
  | Current
  | Workspace
  | Package of string
type event =
  | RegistryPackageLookupStarted of { package: string }
  | RegistryPackageLookupFinished of { package: string; latest_version: string }
  | PackageUpdated of { package: string; from_version: string; to_version: string }
  | ManifestUpdated of { path: Path.t; section: string; operation:
        [
          `Add
          | `Remove
        ]; dependency: string }
  | Pm of Tusk_model.Event.kind
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
  | RegistryInitializationFailed of { registry: string; error: string }
  | RegistryLookupFailed of { package: string; registry: string; error: string }
  | RegistryPackageNotFound of { package: string; registry: string }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of { path: Path.t; error: string }
  | DependencyNotFoundInSection of { path: Path.t; section: string; dependency: string }
  | WorkspaceReloadFailed of { workspace_root: Path.t; error: string }
  | WorkspaceReloadHadErrors of { workspace_root: Path.t; errors: string list }
  | LockRefreshFailed of Error.t
val error_message: error -> string

val add:
  ?on_event:(event -> unit) ->
  workspace:Tusk_model.Workspace.t ->
  cwd:Path.t ->
  request:add_request ->
  unit ->
  (unit, error) result

val remove:
  ?on_event:(event -> unit) ->
  workspace:Tusk_model.Workspace.t ->
  cwd:Path.t ->
  request:remove_request ->
  unit ->
  (unit, error) result

val update: ?on_event:(event -> unit) -> workspace:Tusk_model.Workspace.t -> unit -> (unit, error) result
