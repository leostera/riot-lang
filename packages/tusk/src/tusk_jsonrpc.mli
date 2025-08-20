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

module TuskProtocol :
  Jsonrpc.ApplicationProtocol
    with type request = Rpc.request
     and type response = Rpc.response

(** Server module for RPC request handling *)
module Server : sig
  val create : Miniriot.Pid.t -> (Rpc.request, Rpc.response) Jsonrpc.Server.t
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
  val get_build_graph : t -> (Rpc.build_graph_response, string) result
  val get_workspace_config : t -> (Rpc.workspace_config, string) result
  val build_package : t -> string -> (Rpc.response, string) result
  val build_all : t -> (Rpc.response, string) result
  val restart : t -> (unit, string) result
  val shutdown : t -> (unit, string) result
  val close : t -> unit
end
