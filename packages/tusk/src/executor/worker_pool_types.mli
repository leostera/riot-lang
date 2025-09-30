(** Shared types for worker pool - separated to avoid circular dependencies *)

open Miniriot
open Core
open Model

type ctx = {
  server_pid : Pid.t;
  build_graph : Build_graph.t;
  build_results : Build_results.t;
  workspace : Workspace.t;
  store : Store.t;
}
(** Worker context - all the shared state a worker needs *)

type task = { node : Build_node.t; session_id : Session_id.t }
(** Build task - just the node to build and session *)

(** Worker pool messages *)
type Message.t +=
  | Worker of Message.t  (** Wrapper for worker messages *)
  | WorkerReady of Pid.t
  | Context of ctx  (** Initial context sent to worker *)
  | Task of task
  | TaskCompleted of {
      worker : Pid.t;
      node : Build_node.t;
      artifact : Artifact.t;
    }
  | TaskFailed of { worker : Pid.t; node : Build_node.t; error : string }
  | RequeueWithDependencies of {
      worker : Pid.t;
      node : Build_node.t;
      deps : Build_node.t list;
    }
