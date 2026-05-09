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

val realization_intent: scope -> Riot_model.Package.realization_intent

val dependency_scope: scope -> Riot_model.Package.dependency_scope
