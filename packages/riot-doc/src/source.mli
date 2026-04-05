open Std
open Riot_model

type interface_source = {
  source_path: Path.t;
  relative_path: Path.t;
  module_name: string;
  module_path: string list;
  qualified_name: string;
  content: string;
}
type lookup
val collect_interfaces:
  workspace:Riot_model.Workspace.t ->
  store:Riot_store.Store.t ->
  release:bool ->
  Riot_model.Package.t ->
  (interface_source list, string) result

val build_lookup: interface_source list -> lookup

val find_root_interface: package_name:string -> interface_source list -> interface_source option

val find_by_module_path: lookup -> string list -> interface_source option

val resolve_module_path:
  lookup -> current_path:string list -> target_path:string list -> interface_source option

val source_signature: interface_source list -> string
