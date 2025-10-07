(** Protocol types for communication with the Tusk server *)

open Std
open Miniriot
open Model

(** Target for build operations *)
type target = All | Package of string

module BuildStats = struct
  type t = {
    mutable start_time : Time.Instant.t option;
    mutable end_time : Time.Instant.t option;
    mutable packages_built : int;
    mutable packages_failed : int;
    mutable total_modules : int;
    mutable cache_hits : int;
    mutable cache_misses : int;
  }

  let make () =
    {
      start_time = None;
      end_time = None;
      packages_built = 0;
      packages_failed = 0;
      total_modules = 0;
      cache_hits = 0;
      cache_misses = 0;
    }

  let mark_started t = t.start_time <- Some (Time.Instant.now ())
  let mark_completed t = t.end_time <- Some (Time.Instant.now ())
  let inc_packages_built t = t.packages_built <- t.packages_built + 1
  let inc_packages_failed t = t.packages_failed <- t.packages_failed + 1
  let inc_cache_hits t = t.cache_hits <- t.cache_hits + 1
  let inc_cache_misses t = t.cache_misses <- t.cache_misses + 1
  let set_total_modules t n = t.total_modules <- n

  let get_build_duration t =
    match (t.start_time, t.end_time) with
    | Some start, Some end_ ->
        Time.Duration.to_secs_float
          (Time.Instant.duration_since ~earlier:start end_)
    | _ -> 0.0

  let get_packages_built t = t.packages_built
  let get_packages_failed t = t.packages_failed
  let get_total_modules t = t.total_modules
  let get_cache_hits t = t.cache_hits
  let get_cache_misses t = t.cache_misses
end

(** Request types that can be sent to the server *)
type request =
  | Build of { client_pid : Pid.t; target : target; session_id : Session_id.t }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }
  | GetWorkspaceConfig of { client_pid : Pid.t }
  | GetPackageInfo of { client_pid : Pid.t; package_name : string }
  | GetBuildGraph of { client_pid : Pid.t }
  | FindExecutable of { client_pid : Pid.t; name : string }
  | FindArtifact of {
      client_pid : Pid.t;
      package : string;
      kind : string; (* currently only "binary" *)
      name : string;
    }
  | FormatFile of { client_pid : Pid.t; file_path : Path.t; check_only : bool }
  | FormatCode of {
      client_pid : Pid.t;
      code : string;
      file_path : Path.t option;
    }
  | FormatAll of { client_pid : Pid.t; mode : [ `check | `write ] }
  | NewPackage of {
      client_pid : Pid.t;
      path : Path.t;
      name : string;
      is_library : bool;
    }

(** Response types from the server *)
type response =
  | Pong
  | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_At : Datetime.t;
      stats : BuildStats.t;
    }
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
  | ExecutableFound of { package : string; binary : string }
  | ExecutableNotFound
  | ArtifactFound of { path : Path.t }
  | ArtifactNotFound of { error : string }
  | FormatResult of { formatted_code : string; changed : bool }
  | FormatError of { error : string }
  | FormatAllResult of {
      files_formatted : int;
      files_failed : int;
      errors : (string * string) list;
    }
  | PackageCreated of { path : string; name : string }
  | PackageCreationError of { error : string }
  | PackageNotFound of {
      session_id : Session_id.t;
      package_name : string;
      available_packages : string list;
    }

(** Message types for server communication *)
type Message.t += ServerRequest of request | ServerResponse of response
