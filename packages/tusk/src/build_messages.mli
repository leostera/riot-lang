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
  | BuildAll of Pid.t * bool
        (** Build all packages: (client_pid, is_json_rpc) *)
  | BuildPackage of string * Pid.t * bool
        (** Build specific package: (package_name, client_pid, is_json_rpc) *)
  | NextTask of Pid.t  (** Worker requests next task from server *)
  | TaskComplete of string * bool * Hasher.hash
        (** Worker reports task completion: (package_name, success, hash) *)
  | RequeueWithDependencies of build_task * Build_node.t list
        (** Requeue task with missing dependencies *)
  | Task of build_task  (** Server assigns task to worker *)
  | NoTask  (** Server indicates no tasks available *)
  | Shutdown  (** Server requests worker shutdown *)
  | BuildFinished of { successful : int; failed : int }
        (** Server notifies CLI of build completion *)
