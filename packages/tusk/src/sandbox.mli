(** Sandbox - isolated build execution environment *)

type t
(** Sandbox type representing an isolated build environment *)

type error = string

val create : node:Build_node.t -> workspace:Workspace.t -> t
(** Create a new sandbox for a build graph node *)

val run_actions :
  sandbox:t ->
  store:Store.t ->
  build_graph:Build_graph.t ->
  build_results:Build_results.t ->
  node:Build_node.t ->
  session_id:Session_id.t ->
  (Std.Path.t list, error) result
(** Run actions in sandbox and return the output paths *)

val cleanup : t -> unit
(** Clean up sandbox directory *)

val get_sandbox_dir : t -> string
(** Get the sandbox directory path *)
