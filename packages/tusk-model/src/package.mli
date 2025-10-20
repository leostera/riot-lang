open Std

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type test_module = { name : string; path : Path.t }

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  binaries : binary list;
  library : library option;
  test_library : library option;
  test_modules : test_module list;
}

val hash :
  (module Std.Crypto.Hasher.Intf with type state = 'state) ->
  'state ->
  t ->
  unit

val from_toml :
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, string) result
