open Std

type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Failed of Error.t

type package_result = {
  package: Riot_model.Package_name.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  status: package_status;
  ocamlc_warnings: string list;
}

type t = {
  packages: package_result list;
  summary: Executor.summary;
}

val has_failures: t -> bool

val package_results: t -> package_result list
