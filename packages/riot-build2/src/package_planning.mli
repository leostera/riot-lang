open Std

type t
type input = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  package_hash: Crypto.hash;
}
type artifact_hit = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  artifact: Riot_store.Artifact.t;
}

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  session_id:Riot_model.Session_id.t ->
  parallelism:int ->
  toolchains:Toolchain_service.t ->
  unit ->
  t

val dependency_builds: t -> Goal.build_package -> (Goal.build_package list, Error.t) result

val depset: t -> Goal.build_package -> (Riot_planner.Dependency.t list, Error.t) result

val resolve:
  ?depset:Riot_planner.Dependency.t list ->
  t ->
  Goal.build_package ->
  (input, Error.t) result

val cached_artifact: t -> Goal.build_package -> (artifact_hit option, Error.t) result
