open Miniriot
(** Build message types shared between server and workers *)

type build_task = {
  node : Build_node.t;
  workspace : Workspace.t;
  session_id : Session_id.t option;
}
(** Task description for workers *)
