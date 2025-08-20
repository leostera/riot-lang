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

type build_stats = {
  duration_ms : int;
  packages_built : int;
  packages_failed : int;
  total_modules : int;
  cache_hits : int;
  cache_misses : int;
}
(** Build statistics *)

(** RPC response types *)
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
  | ShutdownAck (* Acknowledgment for shutdown request *)
  | RestartAck (* Acknowledgment for restart request *)
  | Error of string (* Keep for other non-build errors *)

open Miniriot
(** Actor system message wrappers *)

type Message.t +=
  | ClientRequest of Pid.t * request
  | ServerResponse of response
  | RestartServer
  | ShutdownServer
