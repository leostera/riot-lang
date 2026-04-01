open Std
open Tusk_model
open Tusk_planner

(** Package build errors *)
type package_error =
  | PlanningFailed of Tusk_planner.Planning_error.t
  | ExecutionFailed of { message: string; }
  | ActionExecutionFailed of { message: string; }
  | ActionOutputsNotCreated of { missing: Path.t list; }
  | ActionDependenciesFailed of { failed: Graph.SimpleGraph.Node_id.t list; }
val package_error_to_string: package_error -> string

val package_error_to_json: package_error -> Std.Data.Json.t

(** Build status for a package *)
type build_status =
  | Cached of Tusk_store.Artifact.t
  | Built of Tusk_store.Artifact.t
  | Failed of package_error
val build_status_to_json: build_status -> Std.Data.Json.t

(** Result of building a package *)
type build_result = {
  package_key: Package.key;
  package: Package.t;
  status: build_status;
  duration: Time.Duration.t;
}
val build_result_to_json: build_result -> Std.Data.Json.t
(** Collect all source files (.ml, .mli, .c, .h) from a package's src directory.
    
    @param package The package to scan
    @return List of absolute paths to source files
*)
val collect_source_files: Package.t -> Path.t list
(** Build a single package.

    Returns a build_result indicating whether the package was cached, built, or failed.
    
    @param workspace The workspace containing the package
    @param toolchain The OCaml toolchain to use
    @param store The artifact store for caching
    @param package_graph The dependency graph for all packages
    @param package The package to build
*)
val build:
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  build_result
