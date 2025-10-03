open Std
open Model
(** OCamldep wrapper - handles dependency analysis *)

val sort :
  toolchain:Toolchains.toolchain ->
  cwd:Path.t ->
  files:Path.t list ->
  Path.t list
(** Sort ML/MLI files in dependency order *)

val deps :
  toolchain:Toolchains.toolchain ->
  cwd:Path.t ->
  file:Path.t ->
  package_namespace:Model.Namespace.t ->
  Model.Module_name.t list
(** Get dependencies for a single file - returns Module_name.t list *)

val deps_with_flags :
  toolchain:Toolchains.toolchain ->
  cwd:Path.t ->
  file:Path.t ->
  flags:Ocamlc.compiler_flag list ->
  package_namespace:Model.Namespace.t ->
  Model.Module_name.t list
(** Get dependencies for a single file with additional ocamldep flags - returns
    Module_name.t list *)

val all_deps :
  toolchain:Toolchains.toolchain ->
  cwd:Path.t ->
  files:Path.t list ->
  package_namespace:Model.Namespace.t ->
  (Path.t * Model.Module_name.t list) list
(** Get all module dependencies (for building .merlin files) *)
