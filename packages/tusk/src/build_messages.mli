(** Build message types for server-worker communication
    
    This module extends Miniriot's message system with build-specific
    messages for coordinating between the server and worker processes. *)

open Miniriot

(** Build task sent from server to worker *)
type build_task = {
  node : Build_node.t;
  (** Package to build *)
  
  workspace : Workspace.workspace;
  (** Workspace configuration *)
}

(** Extend Miniriot's message type with build messages *)
type Message.t +=
  | ScanWorkspace of string option
  (** Scan workspace, optionally filtering for a target package *)
  
  | BuildAll of Pid.t
  (** Build all packages, includes CLI pid for completion notification *)
  
  | BuildPackage of string * Pid.t
  (** Build specific package: (package_name, cli_pid) *)
  
  | NextTask of Pid.t
  (** Worker requests next task from server *)
  
  | TaskComplete of string * bool * Hasher.hash
  (** Worker reports task completion: (package_name, success, hash) *)
  
  | RequeueWithDependencies of build_task * Build_node.t list
  (** Requeue task with missing dependencies *)
  
  | Task of build_task
  (** Server assigns task to worker *)
  
  | NoTask
  (** Server indicates no tasks available *)
  
  | Shutdown
  (** Server requests worker shutdown *)
  
  | BuildFinished of { successful : int; failed : int }
  (** Server notifies CLI of build completion *)
