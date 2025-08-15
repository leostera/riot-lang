(** JSON-RPC client for tusk server *)

val call : Rpc.request -> (Rpc.response, string) result
(** Connect to the server and send a JSON-RPC request *)

val call_build : Rpc.request -> (string * string list * (Rpc.response, string) result, string) result
(** Connect to the server and send a build request, collecting log messages.
    Returns (session_id, log_messages, final_response) *)

val call_build_streaming : Rpc.request -> (Rpc.response -> unit) -> (Rpc.response, string) result
(** Connect to the server and send a build request, streaming responses via callback.
    The callback is called for each streaming response (BuildStarted, LogOutput).
    Returns the final response (Success or Error). *)

val ping : unit -> (unit, string) result
(** Convenience functions *)

val get_build_graph : unit -> (Rpc.build_graph_response, string) result
val get_workspace_config : unit -> (Rpc.workspace_config, string) result