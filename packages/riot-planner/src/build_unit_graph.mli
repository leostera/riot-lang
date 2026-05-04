open Std
open Riot_model

type request_kind =
  | Runtime
  | Dev of Package.dev_artifacts
type synthetic_tool = {
  package: Package_name.t;
  name: string;
}
type request = {
  roots: Package_name.t list option;
  targets: Target.t list;
  profile: Profile.t;
  kind: request_kind;
  synthetic_tools: synthetic_tool list;
}
type missing_dependency = {
  package: Package_name.t;
  dependency: Package_name.t;
}
type missing_package =
  | Root of Package_name.t
  | Dependency of missing_dependency
type create_error =
  | MissingPackages of {
      missing: missing_package list;
    }
type t
type node = Build_unit.t Graph.SimpleGraph.node

val create: Workspace.t -> request -> (t, create_error) result

val size: t -> int

val keys: t -> Build_unit.key list

val find: t -> Build_unit.key -> node option

val dependencies: t -> Build_unit.key -> Build_unit.key list

val topological_sort: t -> (Build_unit.t list, Build_unit.key list) result
