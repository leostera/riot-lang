(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

(** Method names *)
let method_ping = "tusk.ping"

let method_get_build_graph = "tusk.getBuildGraph"
let method_get_workspace_config = "tusk.getWorkspaceConfig"
let method_build_package = "tusk.buildPackage"
let method_build_all = "tusk.buildAll"
let method_restart = "tusk.restart"
let method_shutdown = "tusk.shutdown"
let method_build_event = "tusk.buildEvent"

(** Helper to create method-specific parameters *)
let build_package_params package =
  Jsonrpc.Named [ ("package", Json.String package) ]
