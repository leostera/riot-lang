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

let build_library = fun ~package ~scope ~profile ~target ->
  BuildLibrary { package; scope; profile; target }

let realization_intent: scope -> Riot_model.Package.realization_intent = fun __tmp1 ->
  match __tmp1 with
  | Build -> Riot_model.Package.Build
  | Runtime -> Riot_model.Package.Runtime
  | Dev -> Riot_model.Package.Dev
  | Run -> Riot_model.Package.Run
  | Test -> Riot_model.Package.Test
  | Bench -> Riot_model.Package.Bench
  | Doc -> Riot_model.Package.Doc
  | Check -> Riot_model.Package.Check

let dependency_scope: scope -> Riot_model.Package.dependency_scope = fun __tmp1 ->
  match __tmp1 with
  | Build -> Riot_model.Package.Build
  | Runtime -> Riot_model.Package.Normal
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check -> Riot_model.Package.Dev

let package_name = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> build.package
  | TestPackage test -> test.package
  | RunBinary run -> run.package

let scope = fun __tmp1 ->
  match __tmp1 with
  | BuildLibrary build -> build.scope
  | TestPackage test -> test.scope
  | RunBinary run -> run.scope

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
