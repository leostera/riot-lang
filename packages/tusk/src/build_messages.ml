(** Build message types shared between server and workers *)
open Miniriot

(** Task description for workers *)
type build_task = {
  node : Build_node.t;
  workspace : Workspace.workspace;
}

(** Extend Miniriot's message type with our custom messages *)
type Message.t += 
  (* CLI -> Server messages *)
  | ScanWorkspace
  | BuildAll of Pid.t  (* includes CLI pid for BuildFinished notification *)
  | BuildPackage of string
  
  (* Worker -> Server messages *)
  | NextTask of Pid.t  (* worker requests next task from server *)
  | TaskComplete of string * bool  (* package name, success *)
  
  (* Server -> Worker messages *)
  | Task of build_task  (* task with node and workspace context *)
  | NoTask  (* no tasks available *)
  | Shutdown
  
  (* Server -> CLI messages *)
  | BuildFinished  (* all builds complete *)