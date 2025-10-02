open Global
open Miniriot

type 'task worker = { pid : Pid.t; task_ref : 'task Ref.t }

(** Internal Messages to Workers *)
type worker_message = Task of Task.t

type Message.t += ToWorker of worker_message

(** Internal Messages to Coordinator *)
type coordinator_message = WorkerReady : 'task worker -> coordinator_message

type Message.t += ToCoordinator of coordinator_message

module PublicMessages = struct
  type Message.t += WorkerReady : 'task worker -> Message.t
end
