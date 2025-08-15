(** JSON-RPC protocol for tusk server *)

type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string; (* "pending" | "building" | "built" | "failed" *)
  deps : string list;
}
(** Build node information *)

type build_graph_response = { nodes : build_node list }
(** Build graph response *)

type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}
(** Workspace configuration *)

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
  | BuildStarted of { session_id : string }
  | LogOutput of { session_id : string; message : string }
  | Error of string
  | Success

val request_to_json : request -> Json.t
(** Serialization *)

val response_to_json : response -> Json.t

val request_of_json : Json.t -> (request, string) result
(** Deserialization *)

val response_of_json : Json.t -> (response, string) result
