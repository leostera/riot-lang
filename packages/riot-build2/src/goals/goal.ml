module Package_target = Package_target
module Build_package = Build_package
module Run_tests = Run_tests
module Run_binary = Run_binary

type package_target = Package_target.t =
  | WorkspaceMembers
  | Package of Riot_model.Package_name.t

type t =
  | BuildPackage of Build_package.t
  | RunTests of Run_tests.t
  | RunBinary of Run_binary.t
