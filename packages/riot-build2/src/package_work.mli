open Std

type build_library = {
  package: Riot_model.Package_name.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type test_package = {
  package: Riot_model.Package_name.t;
  filter: string option;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type run_binary = {
  package: Riot_model.Package_name.t;
  binary: string option;
  args: string list;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type t =
  | BuildLibrary of build_library
  | TestPackage of test_package
  | RunBinary of run_binary

val build_library:
  package:Riot_model.Package_name.t ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  t

val package_name: t -> Riot_model.Package_name.t

val profile: t -> Riot_model.Profile.t

val target: t -> Riot_model.Target.t

val build_key: t -> build_library option
