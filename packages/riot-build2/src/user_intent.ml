open Std

module Package_name = Riot_model.Package_name
module Profile = Riot_model.Profile
module Target = Riot_model.Target

type packages =
  | WorkspaceMembers
  | NamedPackages of Package_name.t list

type targets =
  | HostTarget
  | AllTargets
  | ManyTargets of Target.t list

type profiles =
  | DefaultProfile
  | ManyProfiles of Profile.t list

type build = {
  packages: packages;
  profiles: profiles;
  targets: targets;
  scope: Goal.scope;
}

type test = {
  packages: packages;
  filter: string option;
  profiles: profiles;
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

let build = fun
  ?(packages = WorkspaceMembers)
  ?(profiles = DefaultProfile)
  ?(targets = HostTarget)
  ?(scope = Goal.Runtime)
  () ->
  Build {
    packages;
    profiles;
    targets;
    scope;
  }

let test = fun
  ?(packages = WorkspaceMembers) ?filter ?(profiles = DefaultProfile) ?(targets = HostTarget) () ->
  Test {
    packages;
    filter;
    profiles;
    targets;
  }

let run = fun ~runnable ?(args = []) ?(profile = Profile.release) ?(target = Target.current) () ->
  Run {
    runnable;
    args;
    profile;
    target;
  }
