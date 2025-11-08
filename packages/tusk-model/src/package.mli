open Std

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type sources = { src : Path.t list; native : Path.t list; tests : Path.t list; examples : Path.t list }

type foreign_dependency = {
  name : string;
  path : Path.t;
  build_cmd : string list;
  clean_cmd : string list option;
  test_cmd : string list option;
  outputs : Path.t list;
  env : (string * string) list;
}

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  foreign_dependencies : foreign_dependency list;
  binaries : binary list;
  library : library option;
  sources : sources;
}

val equal : t -> t -> bool

val from_toml :
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, string) result

val to_json : t -> Std.Data.Json.t
val from_json : Std.Data.Json.t -> (t, string) result
