open Model
(** OCamldep wrapper - handles dependency analysis *)

val sort :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  string list
(** Sort ML/MLI files in dependency order *)

val deps :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  file:string ->
  package_namespace:Model.Namespace.t ->
  Model.Module_name.t list
(** Get dependencies for a single file - returns Module_name.t list *)

val deps_with_flags :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  file:string ->
  flags:Ocamlc.compiler_flag list ->
  package_namespace:Model.Namespace.t ->
  Model.Module_name.t list
(** Get dependencies for a single file with additional ocamldep flags - returns
    Module_name.t list *)

val all_deps :
  toolchain:Toolchains.toolchain ->
  cwd:string ->
  files:string list ->
  package_namespace:Model.Namespace.t ->
  (string * Model.Module_name.t list) list
(** Get all module dependencies (for building .merlin files) *)
