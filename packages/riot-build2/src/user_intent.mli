open Std

type packages =
  | AllPackages
  | NamedPackages of Riot_model.Package_name.t list

type targets =
  | HostTarget
  | AllTargets
  | ManyTargets of Riot_model.Target.t list

type build = {
  packages: packages;
  profile: Riot_model.Profile.t;
  targets: targets;
}

type test = {
  packages: packages;
  filter: string option;
  profile: Riot_model.Profile.t;
  targets: targets;
}

type runnable =
  | ByName of string
  | Scoped of {
      package: Riot_model.Package_name.t;
      binary: string option;
    }

type run = {
  runnable: runnable;
  args: string list;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type t =
  | Build of build
  | Test of test
  | Run of run

val build:
  ?packages:packages ->
  ?profile:Riot_model.Profile.t ->
  ?targets:targets ->
  unit ->
  t

val test:
  ?packages:packages ->
  ?filter:string ->
  ?profile:Riot_model.Profile.t ->
  ?targets:targets ->
  unit ->
  t

val run:
  runnable:runnable ->
  ?args:string list ->
  ?profile:Riot_model.Profile.t ->
  ?target:Riot_model.Target.t ->
  unit ->
  t
