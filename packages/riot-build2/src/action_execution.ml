open Std

type ref_ = {
  package: Riot_model.Package_name.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  hash: Crypto.hash;
}

type t = {
  ref_: ref_;
  action: Riot_planner.Action_node.t;
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

let ref_from_action = fun ~package ~profile ~target action ->
  {
    package;
    profile;
    target;
    hash = Riot_planner.Action_node.get_hash action;
  }

let make = fun ~package ~profile ~target ~action ~dependencies ~sandbox_dir ->
  {
    ref_ = ref_from_action ~package ~profile ~target action;
    action;
    dependencies;
    sandbox_dir;
  }

let artifact = fun result ->
  match result.status with
  | Cached artifact
  | Executed artifact -> Some artifact
  | Failed _ -> None
