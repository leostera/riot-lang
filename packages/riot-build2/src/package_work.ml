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

let build_library = fun ~package ~profile ~target -> BuildLibrary { package; profile; target }

let package_name = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> build.package
  | TestPackage test -> test.package
  | RunBinary run -> run.package

let profile = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> build.profile
  | TestPackage test -> test.profile
  | RunBinary run -> run.profile

let target = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> build.target
  | TestPackage test -> test.target
  | RunBinary run -> run.target

let build_key = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> Some build
  | TestPackage _
  | RunBinary _ -> None
