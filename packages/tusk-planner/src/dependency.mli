(** Dependency information for build planning

    Represents a package dependency with its artifact and transitive
    dependencies. *)

open Std
open Std.Collections
open Tusk_model
open Tusk_store

type t = {
  package : Package.t;
  artifact : Artifact.t;
  depset : t list;
  hash : Crypto.hash;
}

val library_cmxa : t -> Path.t
(** Extract the .cmxa library file path from a dependency's artifact *)

val transitive_closure : t list -> t list
(** Flatten dependencies and their transitive deps in dependency-first order,
    deduplicated by package name. *)
