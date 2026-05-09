open Std

module Package_name = Riot_model.Package_name
module Profile = Riot_model.Profile
module Target = Riot_model.Target

type build = {
  packages: Package_name.t list;
  all_packages: bool;
  profile: Profile.t;
  targets: Target.t list;
}

type test = {
  packages: Package_name.t list;
  filter: string option;
  profile: Profile.t;
  targets: Target.t list;
}

type run = {
  package: Package_name.t option;
  binary: string option;
  args: string list;
  profile: Profile.t;
  target: Target.t;
}

type t =
  | Build of build
  | Test of test
  | Run of run

let build =
  fun ?(packages = []) ?(all_packages = false) ?(profile = Profile.debug)
    ?(targets = [ Target.current ]) () ->
  Build { packages; all_packages; profile; targets }

let test =
  fun ?(packages = []) ?filter ?(profile = Profile.debug)
    ?(targets = [ Target.current ]) () ->
  Test { packages; filter; profile; targets }

let run =
  fun ?package ?binary ?(args = []) ?(profile = Profile.debug)
    ?(target = Target.current) () ->
  Run { package; binary; args; profile; target }
