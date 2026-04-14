open Std
open Riot_model

type t = {
  server_pid: Pid.t;
  workspace: Workspace.t;
}
type build_stats = {
  duration_ms: int;
  packages_built: int;
  packages_failed: int;
  total_modules: int;
  cache_hits: int;
  cache_misses: int;
}
type error =
  | StartupFailed of { error: Internal_server.error }
  | PackageNotFound of { package_name: string; available_packages: string list }
  | PackagesNotFound of { package_names: string list; available_packages: string list }
  | BuildFailed of { errors: Riot_executor.Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | UnexpectedEvent of { reason: string }
type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id: Session_id.t;
      completed_at: DateTime.t;
      stats: build_stats;
      results: Riot_executor.Package_builder.build_result list
    }
  | BuildFailed of {
      session_id: Session_id.t;
      failed_at: DateTime.t;
      stats: build_stats;
      built: Riot_executor.Package_builder.build_result list;
      errors: Riot_executor.Package_builder.build_result list
    }
  | PlanningFailed of { session_id: Session_id.t; failed_at: DateTime.t; reason: string }
  | CycleDetected of { session_id: Session_id.t; detected_at: DateTime.t; cycle_nodes: string list }
type build_target =
  | BuildPackage of string
  | BuildPackages of string list
  | BuildAll
type build_scope =
  | Runtime
  | Dev
val error_message: error -> string

val connect_local:
  ?emit:(Riot_model.Event.kind -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  (t, error) result

val connect_local_prepared:
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  (t, error) result

val close: t -> unit

val scan_workspace: t -> current_dir:Path.t -> (unit, 'a) result

module BuildLock: sig
  type t = {
    path: Path.t;
    file: Fs.File.t;
  }
  val retry_interval: Time.Duration.t

  val path: workspace:Workspace.t -> profile:string -> target:Riot_model.Target.t -> Path.t

  val release: t -> unit

  val wait: workspace:Workspace.t -> profile:string -> target:Riot_model.Target.t -> (t, 'a) result

  val acquire:
    workspace:Workspace.t ->
    profile:string ->
    target:Riot_model.Target.t ->
    (unit -> ('a, 'b) result) ->
    ('a, 'b) result
end

val build_streaming:
  t ->
  build_target ->
  ?scope:build_scope ->
  ?profile:string ->
  ?target_arch:Riot_model.Target.t ->
  (streaming_event -> unit) ->
  (streaming_event, error) result

val find_executable: t -> string -> ((string * string) option, 'a) result

val new_package: t -> path:string -> name:string -> is_library:bool -> ((string * string), string) result
