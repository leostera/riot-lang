open Miniriot
(** Build message types shared between server and workers *)

type build_task = {
  node : Build_node.t;
  workspace : Workspace.workspace;
  toolchain_version : string; (* OCaml toolchain version to use *)
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
