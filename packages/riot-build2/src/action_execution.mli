open Std

type ref_ = {
  package: Riot_model.Package_name.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  hash: Crypto.hash;
}

type t = {
  ref_: ref_;
  package: Riot_model.Package.t;
  toolchain: Riot_toolchain.t;
  action: Action.t;
  dependencies: ref_ list;
  sandbox_dir: Path.t;
}

type status =
  | Cached of Riot_store.Artifact.t
  | Executed of Riot_store.Artifact.t
  | Failed of string

type timing = {
  dependency_hashing: Time.Duration.t;
  input_hashing: Time.Duration.t;
  store_lookup: Time.Duration.t;
  cache_promotion: Time.Duration.t;
  sandbox_prepare: Time.Duration.t;
  source_staging: Time.Duration.t;
  command_execution: Time.Duration.t;
  output_verification: Time.Duration.t;
  store_save: Time.Duration.t;
  total: Time.Duration.t;
}

type result = {
  ref_: ref_;
  action_kind: string;
  status: status;
  ocamlc_warnings: string list;
  timing: timing;
}

val empty_timing: timing

val ref_from_action:
  package:Riot_model.Package.t ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  Action.t ->
  ref_

val sandbox_dir_for_ref: base_sandbox_dir:Path.t -> ref_ -> Path.t

val make:
  package:Riot_model.Package.t ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  action:Action.t ->
  dependencies:ref_ list ->
  sandbox_dir:Path.t ->
  t

val artifact: result -> Riot_store.Artifact.t option
