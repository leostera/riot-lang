open Std
open Tusk_model
open Tusk_planner

type Telemetry.event +=
  | BuildStarted of { package : string; target : build_target }
  | BuildCompleted of {
      package : string;
      target : build_target;
      cached : bool;
      duration : Time.Duration.t;
    }
  | BuildFailed of { package : string; target : build_target; error : string }
  | ActionStarted of { package : string; action_kind : string }
  | ActionCompleted of {
      package : string;
      action_kind : string;
      cached : bool;
      duration : Time.Duration.t;
    }
  | ActionFailed of { package : string; action_kind : string; error : string }
  | CacheHit of { package : string; action_kind : string; hash : string }
  | CacheMiss of { package : string; action_kind : string; hash : string }
  | WorkspaceStarted of { target : build_target; package_count : int }
  | WorkspaceCompleted of {
      target : build_target;
      total_duration : Time.Duration.t;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }
