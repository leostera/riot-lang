(** JSON-RPC protocol for tusk server *)

open Miniriot

type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string;
  deps : string list;
}

type build_graph_response = { nodes : build_node list }

type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}

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

type response =
  | Pong
  | BuildGraph of build_graph_response
  | WorkspaceConfig of workspace_config
  | BuildStarted of { session_id : Session_id.t }
  | BuildEvent of { session_id : Session_id.t; log_event : Log.log_event }
  | BuildComplete of build_stats
  | BuildFailed of { stats : build_stats; error : string }
  | ShutdownAck
  | RestartAck
  | Error of string

(** Actor system message wrappers *)
type Message.t +=
  | ClientRequest of Pid.t * request
  | ServerResponse of response
  | RestartServer
  | ShutdownServer
