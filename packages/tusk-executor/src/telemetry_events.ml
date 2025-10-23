open Std
open Tusk_model
open Tusk_planner
open Tusk_store

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
      error : string;
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
