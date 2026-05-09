open Std

module Package_name = Riot_model.Package_name
module Profile = Riot_model.Profile
module Target = Riot_model.Target

type packages =
  | AllPackages
  | NamedPackages of Package_name.t list

type targets =
  | HostTarget
  | AllTargets
  | ManyTargets of Target.t list

type build = {
  packages: packages;
  profile: Profile.t;
  targets: targets;
}

type test = {
  packages: packages;
  filter: string option;
  profile: Profile.t;
  targets: targets;
}

type runnable =
  | ByName of string
  | Scoped of {
      package: Package_name.t;
      binary: string option;
    }

type run = {
  runnable: runnable;
  args: string list;
  profile: Profile.t;
  target: Target.t;
}

type t =
  | Build of build
  | Test of test
  | Run of run

let build =
  fun ?(packages = AllPackages) ?(profile = Profile.debug) ?(targets = HostTarget) () ->
  Build { packages; profile; targets }

let test =
  fun ?(packages = AllPackages) ?filter ?(profile = Profile.debug) ?(targets = HostTarget) () ->
  Test {
    packages;
    filter;
    profile;
    targets;
  }

let run = fun ~runnable ?(args = []) ?(profile = Profile.debug) ?(target = Target.current) () ->
  Run {
    runnable;
    args;
    profile;
    target;
  }
