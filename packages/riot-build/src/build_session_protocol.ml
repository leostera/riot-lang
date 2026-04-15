open Std
open Riot_model

type build_scope =
  Runtime
  | Dev

type target =
  All
  | Package of Package_name.t
  | Packages of Package_name.t list

module BuildStats = struct
  type t = {
    mutable start_time: Time.Instant.t option;
    mutable end_time: Time.Instant.t option;
    mutable packages_built: int;
    mutable packages_failed: int;
    mutable total_modules: int;
    mutable cache_hits: int;
    mutable cache_misses: int;
    mutable action_cache_hits: int;
    mutable action_cache_misses: int;
  }

  let make = fun () ->
    {
      start_time = None;
      end_time = None;
      packages_built = 0;
      packages_failed = 0;
      total_modules = 0;
      cache_hits = 0;
      cache_misses = 0;
      action_cache_hits = 0;
      action_cache_misses = 0;
    }

  let mark_started = fun t -> t.start_time <- Some (Time.Instant.now ())

  let mark_completed = fun t -> t.end_time <- Some (Time.Instant.now ())

  let inc_packages_built = fun t -> t.packages_built <- t.packages_built + 1

  let inc_packages_failed = fun t -> t.packages_failed <- t.packages_failed + 1

  let inc_cache_hits = fun t -> t.cache_hits <- t.cache_hits + 1

  let inc_cache_misses = fun t -> t.cache_misses <- t.cache_misses + 1

  let inc_action_cache_hits = fun t -> t.action_cache_hits <- t.action_cache_hits + 1

  let inc_action_cache_misses = fun t -> t.action_cache_misses <- t.action_cache_misses + 1

  let set_total_modules = fun t n -> t.total_modules <- n

  let get_build_duration = fun t ->
    match (t.start_time, t.end_time) with
    | Some start, Some end_ -> Time.Duration.to_secs_float
      (Time.Instant.duration_since ~earlier:start end_)
    | _ -> 0.0

  let get_packages_built = fun t -> t.packages_built

  let get_packages_failed = fun t -> t.packages_failed

  let get_total_modules = fun t -> t.total_modules

  let get_cache_hits = fun t -> t.cache_hits

  let get_cache_misses = fun t -> t.cache_misses

  let get_action_cache_hits = fun t -> t.action_cache_hits

  let get_action_cache_misses = fun t -> t.action_cache_misses
end

(** Request types exchanged inside the local build session. *)
type request =
  | Build of {
      reply_to: Pid.t;
      target: target;
      scope: build_scope;
      profile: string;
      target_arch: Riot_model.Target.t option;
      session_id: Session_id.t
    }
  | Ping of { reply_to: Pid.t }
  | ScanWorkspace of { reply_to: Pid.t; current_dir: Path.t }
  | GetWorkspaceConfig of { reply_to: Pid.t }
  | GetPackageInfo of { reply_to: Pid.t; package_name: Package_name.t }
  | GetPackageGraph of { reply_to: Pid.t }
  | FindExecutable of { reply_to: Pid.t; name: string }
  | FormatFile of { reply_to: Pid.t; file_path: Path.t; check_only: bool }
  | FormatCode of { reply_to: Pid.t; code: string; file_path: Path.t option }
  | FormatAll of { reply_to: Pid.t; mode: 
        [
          `check
          | `write
        ] }
  | NewPackage of { reply_to: Pid.t; path: Path.t; name: Package_name.t; is_library: bool }

(** Response types emitted by the local build session. *)
type response =
  | Pong
  | WorkspaceScanned
  | BuildStarted of { session_id: Session_id.t; started_at: DateTime.t }
  | BuildEvent of { session_id: Session_id.t; event: Telemetry.event }
  | BuildCompleted of {
      session_id: Session_id.t;
      completed_at: DateTime.t;
      stats: BuildStats.t;
      results: Riot_executor.Package_builder.build_result list
    }
  | BuildFailed of {
      session_id: Session_id.t;
      failed_at: DateTime.t;
      stats: BuildStats.t;
      built: Riot_executor.Package_builder.build_result list;
      errors: Riot_executor.Package_builder.build_result list
    }
  | PlanningFailed of { session_id: Session_id.t; failed_at: DateTime.t; reason: string }
  | CycleDetected of {
      session_id: Session_id.t;
      cycle_nodes: string list;
      detected_at: DateTime.t;  (* List of package names involved in the cycle *)
    }
  | WorkspaceConfig of { workspace: Workspace.t; toolchain: Riot_toolchain.t }
  | PackageInfo of { package: Package.t; sources: Path.t list; dependencies: Package.t list }
  | PackageGraph of { nodes: Package.t list }
  | ExecutableFound of { package: Package_name.t; binary: string }
  | ExecutableNotFound
  | FormatResult of { formatted_code: string; changed: bool }
  | FormatError of { error: string }
  | FormatAllResult of { files_formatted: int; files_failed: int; errors: (string * string) list }
  | PackageCreated of { path: Path.t; name: Package_name.t }
  | PackageCreationError of { error: string }
  | PackageNotFound of {
      session_id: Session_id.t;
      package_name: Package_name.t;
      available_packages: Package_name.t list
    }
  | PackagesNotFound of {
      session_id: Session_id.t;
      package_names: Package_name.t list;
      available_packages: Package_name.t list
    }

(** Message types for local build-session communication. *)
type Message.t +=
  | RequestMessage of request
  | ResponseMessage of response
  | PackageGraphUpdated of Riot_planner.Package_graph.t
