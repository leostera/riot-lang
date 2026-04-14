open Std

type scope = Request.scope =
  | Runtime
  | Dev

type t = {
  workspace: Prepared_workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: scope;
  profile: Riot_model.Profile.t;
}

let make = fun ~workspace ~package_names ~targets ~scope ~profile ->
  { workspace; package_names; targets; scope; profile }

let workspace = fun t -> t.workspace

let package_names = fun t -> t.package_names

let targets = fun t -> t.targets

let scope = fun t -> t.scope

let profile = fun t -> t.profile
