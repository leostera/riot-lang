open Std

type scope =
  | Runtime
  | Dev

type t = {
  workspace: Prepared_workspace.t;
  packages: string list;
  targets: Riot_model.Target.request;
  scope: scope;
  profile: Riot_model.Profile.t;
}

let make = fun ~workspace ~packages ~targets ~scope ~profile () ->
  { workspace; packages; targets; scope; profile }

module Internal = struct
  let workspace = fun t -> t.workspace

  let packages = fun t -> t.packages

  let targets = fun t -> t.targets

  let scope = fun t -> t.scope

  let profile = fun t -> t.profile
end
