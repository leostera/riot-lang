open Std
open Riot_model

(** OCamldep wrapper for dependency analysis. *)
type t

val make: Path.t -> t

val path: t -> Path.t

val sort: t -> cwd:Path.t -> files:Path.t list -> Path.t list

(** Sort ML/MLI files in dependency order *)
val deps: t -> cwd:Path.t -> file:Path.t -> package_namespace:Namespace.t -> Module_name.t list

(** Return dependencies for a single file. *)
val deps_with_flags:
  t ->
  cwd:Path.t ->
  file:Path.t ->
  flags:Ocamlc.compiler_flag list ->
  package_namespace:Namespace.t ->
  Module_name.t list

(**
   Get dependencies for a single file with additional ocamldep flags - returns
   Module_name.t list
*)
val batch_deps:
  t ->
  cwd:Path.t ->
  files:Path.t list ->
  package_namespace:Namespace.t ->
  (Path.t * Module_name.t list) list

(**
   Get dependencies for multiple files in one ocamldep call - much faster than
   calling deps for each file individually
*)
val all_deps:
  t ->
  cwd:Path.t ->
  files:Path.t list ->
  package_namespace:Namespace.t ->
  (Path.t * Module_name.t list) list

(** Get all module dependencies (for building .merlin files) *)
