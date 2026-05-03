open Std
open Riot_model

(**
   Workspace-level package graph planner.

   Builds package dependency graphs and orders packages for execution.
*)
type target =
  | All
  | Package of Package_name.t
  | Packages of Package_name.t list
(** Timing and size counters captured while planning a workspace. *)
type planning_breakdown = {
  manifest_filter_duration: Time.Duration.t;
  filtered_workspace_package_count: int;
  package_graph_duration: Time.Duration.t;
  package_graph_node_count: int;
  package_graph_create_breakdown: Package_graph.create_breakdown;
  target_graph_filter_duration: Time.Duration.t;
  target_graph_node_count: int;
  topological_sort_duration: Time.Duration.t;
  sorted_package_count: int;
}
(** Fully planned package graph for one workspace target. *)
type package_plan = {
  packages: Package.t list;
  nodes: Package_graph.package_node list;
  package_graph: Package_graph.t;
  workspace: Workspace.t;
  breakdown: planning_breakdown;
}
(**
   Plan the workspace build:

   1. Build package dependency graph from workspace 2. Filter to target (All or
   specific package + deps) 3. Detect cycles in package graph 4. Topologically
   sort packages

   Returns the ordered list of packages to build. Does NOT plan module/action
   graphs - that's done lazily per-package by the executor.
*)
type plan_error =
  | PackageNotFound of {
      name: Package_name.t;
      available: Package_name.t list;
    }
  | PackagesNotFound of {
      names: Package_name.t list;
      available: Package_name.t list;
    }
  | CycleDetected of {
      cycle: string list;
    }
  | MissingDependencies of {
      missing: Package_graph.missing_dependency list;
    }
  | PackageLoadFailed of {
      errors: Workspace_manager.load_error list;
    }

val plan_workspace:
  workspace:Workspace.t ->
  target:target ->
  scope:Package_graph.build_scope ->
  load_errors:Workspace_manager.load_error list ->
  dev_artifacts:Package_graph.dev_artifacts ->
  (package_plan, plan_error) result

(** Return the list of packages in the plan, topologically sorted. *)
val packages_in_plan: package_plan -> Package.t list

(** Return the timing and size counters for the plan. *)
val planning_breakdown: package_plan -> planning_breakdown
