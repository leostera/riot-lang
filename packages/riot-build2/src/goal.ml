type package_target =
  | WorkspaceMembers
  | Package of Riot_model.Package_name.t

type binary_target =
  | BinaryByName of string
  | DefaultBinaryInPackage of Riot_model.Package_name.t
  | BinaryInPackage of Riot_model.Package_name.t * string

type build_package = {
  package: package_target;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type run_tests = {
  package: package_target;
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
