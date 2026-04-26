(** Minimal Riot toolchain surface. *)

(** Build and execution actions. *)
module Action = Action

(** Shared constants used across the package. *)
module Const = Const

(** Dependency graph helpers. *)
module Dep_graph = Dep_graph

(** Filesystem scanning helpers. *)
module File_scanner = File_scanner

(** Generic graph utilities. *)
module Graph = Graph

(** I/O helpers. *)
module Io = Io

(** Top-level driver entrypoints. *)
module Driver = Driver

(** OCaml platform integration helpers. *)
module Ocaml_platform = Ocaml_platform

(** Package metadata and helpers. *)
module Package = Package

(** TOML parsing and encoding helpers. *)
module Toml = Toml
