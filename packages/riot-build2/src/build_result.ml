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
  summary: ExecutionSummary.t;
}

let has_failures = fun t -> ExecutionSummary.has_failures t.summary

let package_results = fun t -> t.packages
