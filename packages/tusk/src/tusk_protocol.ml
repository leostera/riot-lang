(** Protocol types for communication with the Tusk server *)

open Miniriot

(** Target for build operations *)
type target = All | Package of string

module BuildStats = struct
  type t = {
    mutable start_time : float;
    mutable end_time : float;
    mutable packages_built : int;
    mutable packages_failed : int;
    mutable total_modules : int;
    mutable cache_hits : int;
    mutable cache_misses : int;
  }
  
  let make () = {
    start_time = 0.0;
    end_time = 0.0;
    packages_built = 0;
    packages_failed = 0;
    total_modules = 0;
    cache_hits = 0;
    cache_misses = 0;
  }
  
  let mark_started t = 
    t.start_time <- Unix.gettimeofday ()
  
  let mark_completed t = 
    t.end_time <- Unix.gettimeofday ()
  
  let inc_packages_built t = 
    t.packages_built <- t.packages_built + 1
  
  let inc_packages_failed t = 
    t.packages_failed <- t.packages_failed + 1
  
  let inc_cache_hits t = 
    t.cache_hits <- t.cache_hits + 1
  
  let inc_cache_misses t = 
    t.cache_misses <- t.cache_misses + 1
  
  let set_total_modules t n = 
    t.total_modules <- n
  
  let get_build_duration t = 
    t.end_time -. t.start_time
  
  let get_packages_built t = t.packages_built
  let get_packages_failed t = t.packages_failed  
  let get_total_modules t = t.total_modules
  let get_cache_hits t = t.cache_hits
  let get_cache_misses t = t.cache_misses
end

(** Request types that can be sent to the server *)
type request =
  | Build of {
      client_pid : Pid.t;
      target : target;
      session_id : Session_id.t option;
    }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }
  | GetWorkspaceConfig of { client_pid : Pid.t }
  | GetPackageInfo of { client_pid : Pid.t; package_name : string }
  | GetBuildGraph of { client_pid : Pid.t }

(** Response types from the server *)
type response =
  | Pong
  | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
  | BuildCompleted of { session_id : Session_id.t; completed_At : Datetime.t; stats: BuildStats.t }
  | CycleDetected of {
      session_id : Session_id.t;
      cycle_nodes : string list;
      detected_at : Datetime.t; (* List of package names involved in the cycle *)
    }
  | WorkspaceConfig of {
      workspace : Workspace.t;
      toolchain : Toolchains.toolchain;
    }
  | PackageInfo of {
      package : Workspace.package;
      sources : Path.t list;
      dependencies : Build_node.t list;
    }
  | BuildGraph of { nodes : Build_node.t list }

(** Message types for server communication *)
type Message.t += ServerRequest of request | ServerResponse of response
