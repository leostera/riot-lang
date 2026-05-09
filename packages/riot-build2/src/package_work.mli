open Std

type scope =
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check

type build_library = {
  package: Riot_model.Package_name.t;
  scope: scope;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type test_package = {
  package: Riot_model.Package_name.t;
  scope: scope;
  filter: string option;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type run_binary = {
  package: Riot_model.Package_name.t;
  scope: scope;
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
  scope:scope ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  t

val realization_intent: scope -> Riot_model.Package.realization_intent

val dependency_scope: scope -> Riot_model.Package.dependency_scope

val package_name: t -> Riot_model.Package_name.t

val scope: t -> scope

val profile: t -> Riot_model.Profile.t

val target: t -> Riot_model.Target.t

val build_key: t -> build_library option
