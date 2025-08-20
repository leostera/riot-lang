(** Sandbox - isolated build execution environment *)

type build_result =
  | Success of string  (** Build succeeded with message *)
  | Failed of string  (** Build failed with error message *)
  | Cached of string  (** Retrieved from cache with message *)

type t = {
  root : string;
  sandbox_dir : string;
  target_dir : string;
  node : Build_node.t;
  workspace : Workspace.t;
}
(** Sandbox type representing an isolated build environment *)

val create : node:Build_node.t -> workspace:Workspace.t -> t
(** Create a new sandbox for a build graph node *)

val get_dependency_includes : t -> string list
(** Get dependency include paths for the sandbox *)

val get_transitive_dependencies :
  Build_node.t -> string list -> Build_node.t list
(** Get all transitive dependencies of a node *)

val copy_dependency_artifacts : t -> unit
(** Copy dependency artifacts into sandbox *)

val run_actions :
  sandbox:t ->
  blueprint:Actions.blueprint ->
  store:Store.t ->
  session_id:Log.session_id option ->
  build_result
(** Run a list of actions in the sandbox *)

val cleanup : t -> unit
(** Clean up sandbox directory *)
