open Std
open Riot_model

(** Legacy typed messages shared by internal [riot-build] components.

    Even though [riot-build] is now a local in-process orchestration layer,
    these request and response types still define the structured contract used
    by some internal runtime pieces.
*)
type build_scope =
  | Runtime
  | Dev
type target =
  | All
  | Package of Package_name.t
  | Packages of Package_name.t list

(** Mutable build statistics recorded during a build session. *)
module BuildStats: sig
  type t

  (** Create an empty statistics accumulator. *)
  val make: unit -> t

  (** Mark the build session as started. *)
  val mark_started: t -> unit

  (** Mark the build session as completed. *)
  val mark_completed: t -> unit

  (** Increment the number of successfully built packages. *)
  val inc_packages_built: t -> unit

  (** Increment the number of failed packages. *)
  val inc_packages_failed: t -> unit

  (** Increment the package-cache hit count. *)
  val inc_cache_hits: t -> unit

  (** Increment the package-cache miss count. *)
  val inc_cache_misses: t -> unit

  (** Increment the action-cache hit count. *)
  val inc_action_cache_hits: t -> unit

  (** Increment the action-cache miss count. *)
  val inc_action_cache_misses: t -> unit

  (** Record the total number of modules involved in the build. *)
  val set_total_modules: t -> int -> unit

  (** Return the measured build duration in seconds. *)
  val get_build_duration: t -> float

  (** Return the number of successfully built packages. *)
  val get_packages_built: t -> int

  (** Return the number of failed packages. *)
  val get_packages_failed: t -> int

  (** Return the total number of modules seen in the build. *)
  val get_total_modules: t -> int

  (** Return the number of package-cache hits. *)
  val get_cache_hits: t -> int

  (** Return the number of package-cache misses. *)
  val get_cache_misses: t -> int

  (** Return the number of action-cache hits. *)
  val get_action_cache_hits: t -> int

  (** Return the number of action-cache misses. *)
  val get_action_cache_misses: t -> int
end

(** Requests sent into the internal build runtime. *)
type request =
  | Build of {
      client_pid: Pid.t;
      target: target;
      scope: build_scope;
      profile: string;
      target_arch: Riot_model.Target.t option;
      session_id: Session_id.t
    }
  | Ping of { client_pid: Pid.t }
  | ScanWorkspace of { client_pid: Pid.t; current_dir: Path.t }
  | GetWorkspaceConfig of { client_pid: Pid.t }
  | GetPackageInfo of { client_pid: Pid.t; package_name: Package_name.t }
  | GetPackageGraph of { client_pid: Pid.t }
  | FindExecutable of { client_pid: Pid.t; name: string }
  | FormatFile of { client_pid: Pid.t; file_path: Path.t; check_only: bool }
  | FormatCode of { client_pid: Pid.t; code: string; file_path: Path.t option }
  | FormatAll of { client_pid: Pid.t; mode: 
        [
          | `check
          | `write
        ] }
  | NewPackage of { client_pid: Pid.t; path: Path.t; name: Package_name.t; is_library: bool }
(** Responses emitted by the internal build runtime. *)
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
  | CycleDetected of { session_id: Session_id.t; cycle_nodes: string list; detected_at: DateTime.t }
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

(** Message constructors used for internal server communication. *)
type Message.t +=
  | ServerRequest of request
  | ServerResponse of response
  | UpdatePackageGraph of Riot_planner.Package_graph.t
