open Std

type scope =
  | Runtime
  | Dev

type t = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: scope;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
}

let make = fun ~workspace ~packages ~targets ~scope ~profile ?(requested_parallelism = None) () ->
  {
    workspace;
    packages;
    targets;
    scope;
    profile;
    requested_parallelism;
  }

module Internal = struct
  let workspace = fun t -> t.workspace

  let packages = fun t -> t.packages

  let targets = fun t -> t.targets

  let scope = fun t -> t.scope

  let profile = fun t -> t.profile

  let requested_parallelism = fun t -> t.requested_parallelism
end
