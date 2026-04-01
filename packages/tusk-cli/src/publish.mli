open Std

type request =
  | Workspace
  | Package of string

type error =
  | ConflictingSelection
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Tusk_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | PublishPlanFailed of Tusk_pm.Publisher.error
  | PublishFailed of { package: string; error: Tusk_pm.Publisher.error }

val command: Std.ArgParser.command

val message: error -> string

val resolve_request:
  package_name:string option ->
  workspace_mode:bool ->
  (request, error) result

val select_packages:
  workspace:Tusk_model.Workspace.t ->
  request ->
  (Tusk_model.Package.t list, error) result

val run: Tusk_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
