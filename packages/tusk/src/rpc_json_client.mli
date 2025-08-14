(** JSON-RPC client for tusk server *)

(** Connect to the server and send a JSON-RPC request *)
val call : Rpc_json.request -> (Rpc_json.response, string) result

(** Convenience functions *)
val ping : unit -> (unit, string) result
val get_build_graph : unit -> (Rpc_json.build_graph_response, string) result
val get_workspace_config : unit -> (Rpc_json.workspace_config, string) result