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

let empty_timing = {
  dependency_hashing = Time.Duration.zero;
  input_hashing = Time.Duration.zero;
  store_lookup = Time.Duration.zero;
  cache_promotion = Time.Duration.zero;
  sandbox_prepare = Time.Duration.zero;
  source_staging = Time.Duration.zero;
  command_execution = Time.Duration.zero;
  output_verification = Time.Duration.zero;
  store_save = Time.Duration.zero;
  total = Time.Duration.zero;
}

let ref_from_action = fun ~package ~profile ~target ~toolchain action ->
  {
    package = package.Riot_model.Package.name;
    profile;
    target;
    hash = Action.hash ~package ~toolchain action;
  }

let sandbox_dir_for_ref = fun ~base_sandbox_dir ref_ ->
  Path.(base_sandbox_dir / Path.v (Crypto.Digest.hex ref_.hash))

let make = fun ~package ~profile ~target ~toolchain ~action ~dependencies ~sandbox_dir ->
  {
    ref_ = ref_from_action ~package ~profile ~target ~toolchain action;
    package;
    toolchain;
    action;
    dependencies;
    sandbox_dir;
  }

let artifact = fun result ->
  match result.status with
  | Cached artifact
  | Executed artifact -> Some artifact
  | Failed _ -> None
