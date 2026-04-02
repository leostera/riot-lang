open Std

type provenance =
  | Workspace
  | Path of Path.t
  | Source of { locator: string; ref_: string option }
  | Registry of { registry: string }
type package_id = {
  registry: string option;
  name: string;
  version: string option;
  sha256: string option;
}
type dependency = {
  name: string;
  package: package_id;
}
type package = {
  id: package_id;
  root: Path.t option;
  provenance: provenance;
  dependencies: dependency list;
  build_dependencies: dependency list;
  dev_dependencies: dependency list;
}
type t = {
  format_version: int;
  dependency_hash: string;
  packages: package list;
}
val of_toml: Std.Data.Toml.value -> (t, string) result

val to_toml: t -> Std.Data.Toml.value

val to_string: t -> string
