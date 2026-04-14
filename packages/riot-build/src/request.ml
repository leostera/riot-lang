open Std

type scope = Build_runtime.build_scope =
  | Runtime
  | Dev

type t = {
  packages: string list;
  targets: Riot_model.Target.request;
  scope: scope;
  profile: Riot_model.Profile.t;
}

let make = fun ~packages ~targets ~scope ~profile () ->
  { packages; targets; scope; profile }

let packages = fun t -> t.packages

let targets = fun t -> t.targets

let scope = fun t -> t.scope

let profile = fun t -> t.profile
