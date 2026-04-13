open Std
open Riot_model

type t
val from_entries:
  namespace:Namespace.t ->
  library_name:string ->
  package_path:Path.t ->
  concrete_library_path:Path.t option ->
  binaries:Package.binary list ->
  Module_scanner.entry list ->
  t

val library_module_name: t -> string

val child_files: t -> Module.t list

val child_modules: t -> Module.t list

val has_concrete_ml: t -> bool

val has_concrete_mli: t -> bool

val concrete_ml_path: t -> Path.t option

val concrete_mli_path: t -> Path.t option

val children_without_lib: t -> Module_scanner.entry list

val deps_for_library_interface: t -> Module.t list
