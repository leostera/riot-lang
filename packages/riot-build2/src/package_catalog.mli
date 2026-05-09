open Std

type t

val create: Riot_model.Workspace.t -> t

val workspace: t -> Riot_model.Workspace.t

val packages: t -> Riot_model.Package.t list

val package_names: t -> Riot_model.Package_name.t list

val find: t -> Riot_model.Package_name.t -> Riot_model.Package.t option

val require: t -> Riot_model.Package_name.t -> (Riot_model.Package.t, Error.t) result

val dependencies: t -> Riot_model.Package.t -> Riot_model.Package.t list

val unsupported_external_dependencies: t -> Riot_model.Package.t -> Riot_model.Package_name.t list
