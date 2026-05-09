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

type result = {
  ref_: ref_;
  status: status;
  ocamlc_warnings: string list;
}

val ref_from_action:
  package:Riot_model.Package.t ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  Action.t ->
  ref_

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
