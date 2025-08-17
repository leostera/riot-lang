(** Tusk RPC Client - High-level client interface *)

(** Client type *)
type t

(** Build request types *)
type build_request = BuildPackage of string | BuildAll

(** Streaming build event *)
type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Json.t
  | BuildFinished of (unit, string) result

(** Create a new Tusk RPC client *)
val create : unit -> t

(** Close the client *)
val close : t -> unit

(** Ping the server *)
val ping : t -> (unit, string) result

(** Get workspace configuration *)
val get_workspace_config : t -> (Rpc.workspace_config, string) result

(** Get build graph *)
val get_build_graph : t -> (Rpc.build_graph_response, string) result

(** Build with streaming events via callback *)
val build_streaming : t -> build_request -> (streaming_event -> unit) -> (streaming_event, string) result

(** Shutdown the server *)
val shutdown : t -> (unit, string) result

(** Restart the server *)
val restart : t -> (unit, string) result