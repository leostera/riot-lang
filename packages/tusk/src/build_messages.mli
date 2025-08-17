(** Build message types for server-worker communication

    This module extends Miniriot's message system with build-specific messages
    for coordinating between the server and worker processes. *)

open Miniriot

type build_task = {
  node : Build_node.t;  (** Package to build *)
  workspace : Workspace.workspace;  (** Workspace configuration *)
}
(** Build task sent from server to worker *)

(** Extend Miniriot's message type with build messages *)
type Message.t +=
  | ScanWorkspace of string option
        (** Scan workspace, optionally filtering for a target package *)
  | BuildAll of { client_pid : Pid.t }  (** Build all packages *)
  | BuildPackage of { package_name : string; client_pid : Pid.t }
        (** Build specific package *)
  | NextTask of { worker_pid : Pid.t }
        (** Worker requests next task from server *)
  | TaskComplete of {
      package_name : string;
      success : bool;
      hash : Hasher.hash;
    }
        (** Worker reports task completion *)
  | RequeueWithDependencies of {
      task : build_task;
      missing_deps : Build_node.t list;
    }
        (** Requeue task with missing dependencies *)
  | Task of build_task  (** Server assigns task to worker *)
  | NoTask  (** Server indicates no tasks available *)
  | Shutdown  (** Server requests worker shutdown *)
  | BuildFinished of { successful : int; failed : int }
        (** Server notifies CLI of build completion *)
