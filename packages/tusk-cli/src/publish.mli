open Std

type request =
  | Workspace
  | Package of string
type error =
  | ConflictingSelection
  | PublishFailed of Tusk_pm.Publish.error
val command: Std.ArgParser.command

val message: error -> string

val resolve_request: package_name:string option -> workspace_mode:bool -> (request, error) result

val run: Tusk_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
