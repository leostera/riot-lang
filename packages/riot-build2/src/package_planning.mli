open Std

type t
type input = {
  build: Package_work.build_library;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  package_hash: Crypto.hash;
}
type artifact_hit = {
  build: Package_work.build_library;
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

val resolve: t -> Package_work.build_library -> (input, Error.t) result

val toolchain_ready: t -> Riot_model.Target.t -> bool

val cached_artifact: t -> Package_work.build_library -> (artifact_hit option, Error.t) result
