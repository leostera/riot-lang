open Std
open Tusk_model

(** Workspace-level planner - builds package dependency graph and orders
    packages for execution *)

type target = All | Package of string

type package_plan = {
  packages : Package.t list;
  package_graph : Package_graph.t;
  workspace : Workspace.t;
}

type plan_error =
  | PackageNotFound of { name : string; available : string list }
  | CycleDetected of { cycle : string list }

val plan_workspace :
  workspace:Workspace.t -> target:target -> (package_plan, plan_error) result
(** Plan the workspace build:

    1. Build package dependency graph from workspace 2. Filter to target (All or
    specific package + deps) 3. Detect cycles in package graph 4. Topologically
    sort packages

    Returns the ordered list of packages to build. Does NOT plan module/action
    graphs - that's done lazily per-package by the executor. *)

val packages_in_plan : package_plan -> Package.t list
(** Get the list of packages in the plan (topologically sorted) *)
