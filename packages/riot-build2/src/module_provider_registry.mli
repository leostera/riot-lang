open Std

type t
type provider = {
  package: Riot_model.Package_name.t;
  root_module: string;
  build: Goal.build_package;
  key: Work_node.key;
}

val create: catalog:Package_catalog.t -> unit -> t

val providers_for_build: t -> Goal.build_package -> (provider list, Error.t) result

val find_for_build:
  t ->
  Goal.build_package ->
  root_module:string ->
  (provider option, Error.t) result

val dependency_keys_for_build: t -> Goal.build_package -> (Work_node.key list, Error.t) result
