(** Dependency-aware package planning and hashing. *)
open Std
open Riot_model

(**
   Plan a package:

   1. Compute the build-unit input hash from the package, build context, and
   dependency artifact outputs. 2. Try the warm cached-artifact +
   export-manifest fast path. 3. Otherwise try to load a cached plan bundle by
   that hash. 4. On miss, build module/action graphs and persist the bundle. 5.
   Return Cached or Planned.

   The hash includes:
   - Package metadata (name, dependencies, binaries)
   - Build profile and target context
   - Source-level package metadata
   - Hashes of all direct dependencies (transitive cache invalidation)
*)
type plan_result =
  | Cached of {
      unit_key: Build_unit.key;
      package: Package.t;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
      breakdown: planning_breakdown;
    }
  | Planned of {
      unit_key: Build_unit.key;
      package: Package.t;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      depset: Dependency.t list;
      sandbox_files: Sandbox_file.t list;
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

type input_hash_cache

val create_input_hash_cache: unit -> input_hash_cache

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

val plan_build_unit:
  on_source_analyzed:(Module_graph.source_analysis_progress -> unit) ->
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  unit:Build_unit.t ->
  depset:Dependency.t list ->
  build_ctx:Build_ctx.t ->
  (plan_result, Planning_error.t) result

val plan_build_unit_with_cache:
  on_source_analyzed:(Module_graph.source_analysis_progress -> unit) ->
  input_hash_cache:input_hash_cache ->
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  unit:Build_unit.t ->
  depset:Dependency.t list ->
  build_ctx:Build_ctx.t ->
  (plan_result, Planning_error.t) result

val plan_build_unit_with_cache_and_source_analyzer:
  analyze_sources:Module_graph.source_analyzer ->
  on_source_analyzed:(Module_graph.source_analysis_progress -> unit) ->
  input_hash_cache:input_hash_cache ->
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  unit:Build_unit.t ->
  depset:Dependency.t list ->
  build_ctx:Build_ctx.t ->
  (plan_result, Planning_error.t) result
