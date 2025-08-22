(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

val method_ping : string
(** Method names *)

val method_get_build_graph : string
val method_get_workspace_config : string
val method_build_package : string
val method_build_all : string
val method_restart : string
val method_shutdown : string
val method_build_event : string

val build_package_params : string -> Jsonrpc.params
(** Helper to create method-specific parameters *)

module TuskProtocol : sig
  type build_node = {
    package_name : string;
    src_dir : string;
    out_dir : string;
    status : string;
    deps : string list;
  }

  type build_graph_response = { nodes : build_node list }

  type workspace_config = {
    workspace_root : string;
    toolchain : string;
    packages : string list;
  }

  type request =
    | Ping
    | GetBuildGraph
    | GetWorkspaceConfig
    | BuildPackage of string
    | BuildAll
    | Restart
    | Shutdown

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
    | BuildStarted of { session_id : Session_id.t }
    | BuildEvent of { session_id : Session_id.t; log_event : Log.log_event }
    | BuildComplete of { session_id : Session_id.t; stats : build_stats }
    | BuildFailed of {
        session_id : Session_id.t;
        stats : build_stats;
        error : string;
      }
    | ShutdownAck
    | RestartAck
    | Error of string

  include Jsonrpc.ApplicationProtocol
    with type request := request
     and type response := response
end

(** Server module for RPC request handling *)
module Server : sig
  val create : Miniriot.Pid.t -> (TuskProtocol.request, TuskProtocol.response) Jsonrpc.Server.t
  (** Create a JSON-RPC server that handles tusk requests *)
end

(** Client module for RPC communication *)
module Client : sig
  type t

  (** Streaming build event *)
  type streaming_event =
    | BuildStarted of Session_id.t
    | BuildEvent of Log.log_event
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
  val get_build_graph : t -> (TuskProtocol.build_graph_response, string) result
  val get_workspace_config : t -> (TuskProtocol.workspace_config, string) result
  val build_package : t -> string -> (TuskProtocol.response, string) result
  val build_all : t -> (TuskProtocol.response, string) result
  val restart : t -> (unit, string) result
  val shutdown : t -> (unit, string) result
  val close : t -> unit
end