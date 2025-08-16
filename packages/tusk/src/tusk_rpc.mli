(** Tusk RPC Protocol - JSON-RPC 2.0 compliant *)

(** Method names *)
val method_ping : string
val method_get_build_graph : string
val method_get_workspace_config : string
val method_build_package : string
val method_build_all : string
val method_restart : string
val method_shutdown : string

(** Response types *)
type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string;
  deps : string list;
}

type build_graph_response = { 
  nodes : build_node list 
}

type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}

type build_started = {
  session_id : string;
}

type log_output = {
  session_id : string;
  message : string;
}

(** Convert to/from JSON *)
val build_node_to_json : build_node -> Json.t
val build_node_of_json : Json.t -> (build_node, string) result

val build_graph_to_json : build_graph_response -> Json.t
val build_graph_of_json : Json.t -> (build_graph_response, string) result

val workspace_config_to_json : workspace_config -> Json.t
val workspace_config_of_json : Json.t -> (workspace_config, string) result

val build_started_to_json : build_started -> Json.t
val build_started_of_json : Json.t -> (build_started, string) result

val log_output_to_json : log_output -> Json.t
val log_output_of_json : Json.t -> (log_output, string) result

(** Helper to create method-specific parameters *)
val build_package_params : string -> Jsonrpc.params