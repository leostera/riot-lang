open Std
open Tusk_model
open Tusk_planner
open Tusk_store

(** Error types for package builds *)
type package_error =
  | PlanningFailed of Planning_error.t
  | ExecutionFailed of { message : string }
  | ActionExecutionFailed of { message : string }
  | ActionOutputsNotCreated of { missing : Path.t list }
  | ActionDependenciesFailed of { failed : Graph.SimpleGraph.Node_id.t list }

(** Telemetry events for build system operations.
    
    These events extend the Std.Telemetry.event type and provide
    detailed information about build progress, cache hits/misses,
    and failures. *)
type Telemetry.event +=
  | BuildStarted of { package : Package.t; target : Workspace_planner.target }
  | BuildCompleted of {
      package : Package.t;
      target : Workspace_planner.target;
      status : [ `Fresh | `Cached ];
      duration : Time.Duration.t;
    }
  | BuildFailed of {
      package : Package.t;
      target : Workspace_planner.target;
      error : package_error;
    }
  | BuildSkipped of {
      package : Package.t;
      target : Workspace_planner.target;
      reason : string;
    }
  | ActionStarted of { package : Package.t; action : Action_node.t }
  | ActionCompleted of {
      package : Package.t;
      action : Action_node.t;
      artifact : Artifact.t;
      status : [ `Fresh | `Cached ];
      duration : Time.Duration.t;
    }
  | ActionFailed of {
      package : Package.t;
      action : Action_node.t;
      error : string;
    }
  | CacheHit of {
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | CacheMiss of {
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | WorkspaceStarted of {
      target : Workspace_planner.target;
      package_count : int;
    }
  | WorkspaceCompleted of {
      target : Workspace_planner.target;
      total_duration : Time.Duration.t;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }

(** Convert a telemetry event to JSON.
    
    Returns [Some json] if the event is one of the build telemetry events,
    or [None] if it's a different type of event. *)
val to_json : Telemetry.event -> Data.Json.t option

(** Parse a telemetry event from JSON.
    
    Note: Action-related events (ActionStarted, ActionCompleted, etc.) cannot
    be fully deserialized because they contain Action_node.t which is not
    serializable. *)
val from_json : Data.Json.t -> (Telemetry.event, Data.Json.t) result
