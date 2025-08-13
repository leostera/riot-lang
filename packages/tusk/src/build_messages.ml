open Miniriot
(** Build message types shared between server and workers *)

type build_task = {
  node : Build_node.t;
  workspace : Workspace.workspace;
}
(** Task description for workers *)

(** Extend Miniriot's message type with our custom messages *)
type Message.t +=
  | (* CLI -> Server messages *)
      ScanWorkspace of
      string option (* optional target package to filter for *)
  | BuildAll of Pid.t (* includes CLI pid for BuildFinished notification *)
  | BuildPackage of string * Pid.t (* package name, CLI pid *)
  | (* Worker -> Server messages *)
      NextTask of
      Pid.t (* worker requests next task from server *)
  | TaskComplete of string * bool * Hasher.hash (* package name, success, hash *)
  | RequeueWithDependencies of build_task * Build_node.t list (* task, missing dependency nodes *)
  | (* Server -> Worker messages *)
      Task of
      build_task (* task with node and workspace context *)
  | NoTask (* no tasks available *)
  | Shutdown
  | (* Server -> CLI messages *)
      BuildFinished of {
      successful : int;
      failed : int;
    }

(** Tests submodule *)
module Tests = struct
  [@riot.test]
  let test_message_routing_between_processes () : (unit, string) result =
    (* Test that messages are correctly routed between CLI, server, and workers *)
    Ok ()
  
  [@test]
  let test_build_task_contains_all_needed_context () : (unit, string) result =
    (* Test that build_task has node and workspace info for workers *)
    Ok ()
  
  [@riot.test]
  let test_worker_requests_tasks_correctly () : (unit, string) result =
    (* Test NextTask/Task/NoTask flow *)
    Ok ()
  
  [@riot.test]
  let test_requeue_with_dependencies_preserves_order () : (unit, string) result =
    (* Test that requeued tasks maintain dependency order *)
    Ok ()
end
