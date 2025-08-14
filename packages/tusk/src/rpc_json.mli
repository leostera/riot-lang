(** JSON-based RPC protocol for tusk server *)

(** Build node information *)
type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string;  (* "pending" | "building" | "built" | "failed" *)
  deps : string list;
}

(** Build graph response *)
type build_graph_response = {
  nodes : build_node list;
}

(** Workspace configuration *)
type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}

(** RPC request types *)
type request =
  | Ping
  | GetBuildGraph
  | GetWorkspaceConfig
  | BuildPackage of string
  | BuildAll
  | Restart
  | Shutdown

(** RPC response types *)
type response =
  | Pong
  | BuildGraph of build_graph_response
  | WorkspaceConfig of workspace_config
  | Error of string
  | Success

(** Serialization *)
val request_to_json : request -> Json.t
val response_to_json : response -> Json.t

(** Deserialization *)
val request_of_json : Json.t -> (request, string) result
val response_of_json : Json.t -> (response, string) result