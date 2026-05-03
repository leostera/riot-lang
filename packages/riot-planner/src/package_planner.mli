(** Dependency-aware package planning and hashing. *)
open Std
open Riot_model

(**
   Plan a package:

   1. Check if all package dependencies are planned in the package_graph 2. If
   not → return MissingDependencies 3. Compute package input hash 4. Try the
   warm cached-artifact + export-manifest fast path 5. Otherwise try to load a
   cached plan bundle by that hash 6. On miss, build module/action graphs and
   persist the bundle 7. Return Cached or Planned

   The hash includes:
   - Package metadata (name, dependencies, binaries)
   - Build profile and target context
   - Source-level package metadata
   - Hashes of all direct dependencies (transitive cache invalidation)
*)
type plan_result =
  | Cached of {
      package_key: Package.key;
      package: Package.t;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
      breakdown: planning_breakdown;
    }
  | Planned of {
      package_key: Package.key;
      package: Package.t;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      depset: Dependency.t list;
      breakdown: planning_breakdown;
    }
  | MissingDependencies of {
      package: Package.t;
      missing: Package.t list;
      breakdown: planning_breakdown;
    }
  | FailedDependencies of {
      package: Package.t;
      failed: Package.t list;
      breakdown: planning_breakdown;
    }

and planning_breakdown = {
  dependency_count: int;
  dependency_check_duration: Time.Duration.t;
  input_hash_duration: Time.Duration.t;
  artifact_lookup_duration: Time.Duration.t;
  artifact_cache_hit: bool;
  plan_bundle_lookup_duration: Time.Duration.t;
  plan_bundle_decode_duration: Time.Duration.t;
  plan_bundle_cache_hit: bool;
  module_plan_duration: Time.Duration.t;
}

val compute_input_hash:
  ?planner_version:string ->
  package:Package.t ->
  depset:Dependency.t list ->
  workspace:Workspace.t ->
  profile:Profile.t ->
  build_ctx:Build_ctx.t ->
  toolchain:Riot_toolchain.t ->
  unit ->
  Std.Crypto.hash

val plan_package:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  (plan_result, Planning_error.t) result
