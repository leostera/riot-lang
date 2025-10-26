open Std
open Tusk_model

val method_ping : string
val method_get_build_graph : string
val method_get_workspace_config : string
val method_get_package_info : string
val method_build_package : string
val method_build_all : string
val method_restart : string
val method_shutdown : string
val method_build_event : string
val method_format_file : string
val method_format_code : string
val method_format_all : string
val method_new_package : string
val method_find_executable : string
val method_find_artifact : string
val build_package_params : string -> Jsonrpc.params

module WireProtocol : sig
  type build_node = {
    package_name : string;
    src_dir : string;
    out_dir : string;
    status : string;
    deps : string list;
  }

  type build_graph_response = { nodes : build_node list }

  type package_info = {
    name : string;
    path : string;
    dependencies : string list;
  }

  type workspace_config = {
    workspace_root : string;
    target_dir : string;
    toolchain : string;
    toolchain_path : string;
    packages : package_info list;
    total_packages : int;
  }

  type package_detail = {
    package : package_info;
    sources : string list;
    dependency_names : string list;
  }

  type request =
    | Ping
    | GetBuildGraph
    | GetWorkspaceConfig
    | GetPackageInfo of string
    | BuildPackage of string
    | BuildAll
    | Restart
    | Shutdown
    | FindExecutable of string
    | FindArtifact of { package : string; kind : string; name : string }
    | FormatFile of { file_path : string; check_only : bool }
    | FormatCode of { code : string; file_path : string option }
    | FormatAll of { mode : [ `check | `write ] }
    | NewPackage of { path : string; name : string; is_library : bool }

  type build_stats = {
    duration_ms : int;
    packages_built : int;
    packages_failed : int;
    total_modules : int;
    cache_hits : int;
    cache_misses : int;
  }

  type package_error =
    | PlanningFailed of Tusk_planner.Planning_error.t
    | ExecutionFailed of { message : string }
    | ActionFailed of Tusk_executor.Action_executor.action_error

  type build_status =
    | Cached of Tusk_store.Artifact.t
    | Built of Tusk_store.Artifact.t
    | Failed of package_error

  type build_result = {
    package : Package.t;
    status : build_status;
    duration : Std.Time.Duration.t;
  }

  type response =
    | Pong
    | BuildGraph of build_graph_response
    | WorkspaceConfig of workspace_config
    | PackageInfo of package_detail
    | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
    | BuildEvent of { session_id : Session_id.t; event : Telemetry.event }
    | CycleDetected of {
        session_id : Session_id.t;
        detected_at : Datetime.t;
        cycle_nodes : string list;
      }
    | BuildComplete of {
        session_id : Session_id.t;
        completed_at : Datetime.t;
        stats : build_stats;
        results : build_result list;
      }
    | BuildFailed of {
        session_id : Session_id.t;
        failed_at : Datetime.t;
        stats : build_stats;
        built : build_result list;
        errors : build_result list;
      }
    | ExecutableFound of { package : string; binary : string }
    | ExecutableNotFound
    | ArtifactFound of { path : string }
    | ArtifactNotFound of { error : string }
    | ShutdownAck
    | RestartAck
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
    | Error of string

  include
    Jsonrpc.ApplicationProtocol
      with type request := request
       and type response := response
end
