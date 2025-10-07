(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

open Std
open Core

val method_ping : string
(** Method names *)

val method_get_build_graph : string
val method_get_workspace_config : string
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
(** Helper to create method-specific parameters *)

module WireProtocol : sig
  (** External RPC Wire Protocol - simple, JSON-serializable types for external
      clients *)

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

  type response =
    | Pong
    | BuildGraph of build_graph_response
    | WorkspaceConfig of workspace_config
    | PackageInfo of package_detail
    | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
    | BuildEvent of { session_id : Session_id.t; event : Event.t }
    | CycleDetected of {
        session_id : Session_id.t;
        detected_at : Datetime.t;
        cycle_nodes : string list;
      }
    | BuildComplete of {
        session_id : Session_id.t;
        completed_at : Datetime.t;
        stats : build_stats;
      }
    | BuildFailed of {
        session_id : Session_id.t;
        failed_at : Datetime.t;
        stats : build_stats;
        error : string;
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

(** Server module for RPC request handling *)
module Server : sig
  val create :
    Miniriot.Pid.t ->
    (WireProtocol.request, WireProtocol.response) Jsonrpc.Server.t
  (** Create a JSON-RPC server that handles tusk requests *)
end

(** Client module for RPC communication *)
module Client : sig
  type t

  (** Streaming build event *)
  type streaming_event =
    | BuildStarted of Session_id.t
    | BuildEvent of Event.t
    | BuildFinished of (unit, string) result

  (** Build request type *)
  type build_request = BuildPackage of string | BuildAll

  val create : host:string -> port:int -> (t, string) result

  val build_streaming :
    t ->
    build_request ->
    (streaming_event -> unit) ->
    (streaming_event, string) result

  val ping : t -> (unit, string) result
  val get_build_graph : t -> (WireProtocol.build_graph_response, string) result
  val get_workspace_config : t -> (WireProtocol.workspace_config, string) result

  val get_package_info :
    t -> string -> (WireProtocol.package_detail, string) result

  val build_package : t -> string -> (WireProtocol.response, string) result
  val build_all : t -> (WireProtocol.response, string) result
  val find_executable : t -> string -> ((string * string) option, string) result

  val find_artifact :
    t -> package:string -> kind:string -> name:string -> (string, string) result

  val restart : t -> (unit, string) result
  val shutdown : t -> (unit, string) result

  val format_file :
    t -> file_path:string -> check_only:bool -> (string * bool, string) result

  val format_code :
    t ->
    code:string ->
    file_path:string option ->
    (string * bool, string) result

  val format_all :
    t ->
    mode:[ `check | `write ] ->
    (int * int * (string * string) list, string) result

  val new_package :
    t ->
    path:string ->
    name:string ->
    is_library:bool ->
    (string * string, string) result

  val create_package :
    t ->
    name:string ->
    deps:string list ->
    is_library:bool ->
    (string * string list, string) result

  val create_module :
    t ->
    package:string ->
    module_name:string ->
    contents:string ->
    (string list, string) result

  val close : t -> unit
end
