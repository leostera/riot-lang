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
