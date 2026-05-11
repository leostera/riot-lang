open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  store:Riot_store.Store.t ->
  unit ->
  t

val begin_execution: t -> unit

val check_dir: Path.t

val link_dir: Path.t

val deps_dir: Riot_model.Package_name.t -> Path.t

val dep_check_dir: Riot_model.Package_name.t -> Path.t

val dep_link_dir: Riot_model.Package_name.t -> Path.t

val prepare:
  t ->
  Package_planning.input ->
  depset:Riot_planner.Dependency.t list ->
  (Path.t, Error.t) result

val cleanup_success: t -> Goal.build_package -> unit
