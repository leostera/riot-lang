open Std

type provenance =
  | Workspace
  | Path of Path.t
  | Source of {
      locator: string;
      ref_: string option;
    }
  | Registry of { registry: string }
type package_id = {
  registry: string option;
  name: Package_name.t;
  version: string option;
  sha256: string option;
}
type dependency = {
  name: Package_name.t;
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
type container =
  | Lockfile
  | Package
  | PackageId
  | Dependency
  | DependencyList
  | Provenance
type error =
  | ExpectedTable of {
      container: container;
    }
  | ExpectedArray of {
      container: container;
    }
  | MissingField of {
      container: container;
      field: string;
    }
  | InvalidFieldType of {
      container: container;
      field: string;
      expected: string;
    }
  | InvalidPackageName of {
      container: container;
      field: string;
      value: string;
      error: Package_name.error;
    }
  | UnknownProvenanceKind of { value: string }

val error_message: error -> string

val from_toml: Std.Data.Toml.value -> (t, error) result

val to_toml: t -> Std.Data.Toml.value

val to_string: t -> string
