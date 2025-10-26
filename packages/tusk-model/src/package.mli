open Std

type dependency_source = Workspace | Path of Path.t
type dependency = { name : string; source : dependency_source }
type binary = { name : string; path : Path.t }
type library = { path : Path.t }
type sources = { src : Path.t list; native : Path.t list; tests : Path.t list }

type t = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
  binaries : binary list;
  library : library option;
  sources : sources;
}

val from_toml :
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, string) result

val to_json : t -> Std.Data.Json.t
