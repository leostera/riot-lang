type scope =
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check

type binary_target =
  | BinaryByName of string
  | DefaultBinaryInPackage of Riot_model.Package_name.t
  | BinaryInPackage of Riot_model.Package_name.t * string

type build_package = {
  package: Riot_model.Package_name.t;
  scope: scope;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type run_tests = {
  package: Riot_model.Package_name.t;
  filter: string option;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type run_binary = {
  binary: binary_target;
  args: string list;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type t =
  | BuildPackage of build_package
  | RunTests of run_tests
  | RunBinary of run_binary

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
