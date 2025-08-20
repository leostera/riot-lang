(** Build message types for server-worker communication

    This module extends Miniriot's message system with build-specific messages
    for coordinating between the server and worker processes. *)

open Miniriot

type build_task = {
  node : Build_node.t;  (** Package to build *)
  workspace : Workspace.t;  (** Workspace configuration *)
  session_id : Session_id.t option;  (** Build session ID for logging *)
}
(** Build task sent from server to worker *)
