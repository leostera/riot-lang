(** Protocol types for communication with the Tusk server *)

open Miniriot

(** Target for build operations *)
type target = All | Package of string

(** Request types that can be sent to the server *)
type request =
  | Build of {
      client_pid : Pid.t;
      target : target;
      session_id : Session_id.t option;
    }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }
  | GetWorkspaceConfig of { client_pid : Pid.t }
  | GetPackageInfo of { client_pid : Pid.t; package_name : string }
  | GetBuildGraph of { client_pid : Pid.t }

(** Response types from the server *)
type response =
  | Pong
  | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
  | BuildCompleted of { session_id : Session_id.t; completed_At : Datetime.t }
  | CycleDetected of {
      session_id : Session_id.t;
      cycle_nodes : string list;
      detected_at : Datetime.t;
    }
  | WorkspaceConfig of {
      workspace : Workspace.t;
      toolchain : Toolchains.toolchain;
    }
  | PackageInfo of {
      package : Workspace.package;
      sources : Path.t list;
      dependencies : Build_node.t list;
    }
  | BuildGraph of { nodes : Build_node.t list }

(** Message types for server communication *)
type Message.t += ServerRequest of request | ServerResponse of response
