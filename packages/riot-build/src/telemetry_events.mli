open Std
open Riot_model
open Riot_planner
open Riot_store

(** Error types for package builds *)
type package_error =
  | PlanningFailed of Planning_error.t
  | ExecutionFailed of { message: string }
  | ActionExecutionFailed of { message: string }
  | ActionOutputsNotCreated of {
      missing: Path.t list;
    }
  | ActionDependenciesFailed of {
      failed: Graph.SimpleGraph.Node_id.t list;
    }
type package_planning_status = [ | `Planned | `MissingDependencies | `FailedDependencies | `Failed]
type package_planning_breakdown = {
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
type workspace_graph_breakdown = {
  build_node_realization_count: int;
  build_node_realization_duration: Time.Duration.t;
  runtime_node_realization_count: int;
  runtime_node_realization_duration: Time.Duration.t;
  dev_node_realization_count: int;
  dev_node_realization_duration: Time.Duration.t;
  edge_wiring_duration: Time.Duration.t;
}
type subject =
  | All
  | Package of Package_name.t
  | Packages of Package_name.t list
type warning_source = [ | `Fresh | `Cached]
(**
   Telemetry events for build system operations.

   These events extend the Std.Telemetry.event type and provide
   detailed information about build progress, cache hits/misses,
   and failures.
*)
type Telemetry.event +=
  | PackageStarted of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      started_at: Time.Instant.t;
    }
  | WorkspacePlanStarted of {
      session_id: Session_id.t;
      target: subject;
      workspace_package_count: int;
    }
  | WorkspacePlanCompleted of {
      session_id: Session_id.t;
      target: subject;
      workspace_package_count: int;
      planned_package_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceManifestFilterCompleted of {
      session_id: Session_id.t;
      target: subject;
      filtered_workspace_package_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceGraphCreated of {
      session_id: Session_id.t;
      target: subject;
      node_count: int;
      breakdown: workspace_graph_breakdown;
      duration: Time.Duration.t;
    }
  | WorkspaceTargetGraphFiltered of {
      session_id: Session_id.t;
      target: subject;
      node_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceTopologicalSortCompleted of {
      session_id: Session_id.t;
      target: subject;
      sorted_package_count: int;
      duration: Time.Duration.t;
    }
  | PlanningWorkspaceStarted of {
      session_id: Session_id.t;
      target: subject;
      package_count: int;
    }
  | PlanningWorkspaceCompleted of {
      session_id: Session_id.t;
      target: subject;
      duration: Time.Duration.t;
      planned_count: int;
      missing_count: int;
      failed_count: int;
    }
  | PackagePlanningResult of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      status: package_planning_status;
      duration: Time.Duration.t;
      reason: string option;
    }
  | PackagePlanningBreakdown of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      breakdown: package_planning_breakdown;
    }
  | CompilationStarted of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      action_count: int;
      started_at: Time.Instant.t;
    }
  | SandboxCreated of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      path: Path.t;
      created_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | SandboxInputsCopied of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      input_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | SandboxDependenciesCopied of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      dependency_count: int;
      object_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackageExecutionPrepared of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      input_count: int;
      dependency_count: int;
      dependency_object_count: int;
      prepared_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackageOcamlcWarnings of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      source: warning_source;
      messages: string list;
    }
  | BuildCompleted of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      status: [`Fresh | `Cached];
      duration: Time.Duration.t;
    }
  | BuildFailed of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      error: package_error;
    }
  | BuildSkipped of {
      session_id: Session_id.t;
      package: Package.t;
      target: subject;
      build_target: Target.t;
      reason: string;
    }
  | ActionStarted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      started_at: Time.Instant.t;
    }
  | ActionCommandStarted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      started_at: Time.Instant.t;
      command: string;
    }
  | ActionCompleted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      completed_at: Time.Instant.t;
      artifact: Artifact.t;
      status: [`Fresh | `Cached];
      duration: Time.Duration.t;
    }
  | ActionFailed of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      failed_at: Time.Instant.t;
      error: string;
    }
  | CacheHit of {
      session_id: Session_id.t;
      package: Package.t;
      action: Action_node.t;
      hash: Crypto.hash;
    }
  | CacheMiss of {
      session_id: Session_id.t;
      package: Package.t;
      action: Action_node.t;
      hash: Crypto.hash;
    }
  | WorkspaceStarted of {
      session_id: Session_id.t;
      target: subject;
      package_count: int;
    }
  | WorkspaceCompleted of {
      session_id: Session_id.t;
      target: subject;
      total_duration: Time.Duration.t;
      cached_count: int;
      built_count: int;
      failed_count: int;
    }

(**
   Convert a telemetry event to JSON.

   Returns [Some json] if the event is one of the build telemetry events,
   or [None] if it's a different type of event.
*)
val to_json: Telemetry.event -> Data.Json.t option

(** Return the session id for build telemetry events. *)
val event_session_id: Telemetry.event -> Riot_model.Session_id.t option

(** Return the semantic timestamp for build telemetry events that carry one. *)
val event_timestamp: Telemetry.event -> (string * Time.Instant.t) option

(**
   Parse a telemetry event from JSON.

   Note: Action-related events (ActionStarted, ActionCompleted, etc.) can be
   emitted for logging and debugging, but they are still not currently
   reconstructed to full event values during parsing because Action_node.t does
   not yet have a reverse deserialization path.
*)
val from_json: Data.Json.t -> (Telemetry.event, Data.Json.t) result
