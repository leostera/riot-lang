open Std

type request =
  | Workspace
  | Package of Riot_model.Package_name.t

type error =
  | ConflictingSelection
  | PublishFailed of Riot_publish.publish_error

val command: Std.ArgParser.command

val message: error -> string

val resolve_request: package_name:Riot_model.Package_name.t option -> workspace_mode:bool -> (request, error) result

val run: Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
