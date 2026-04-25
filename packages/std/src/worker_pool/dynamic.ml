(**
   Dynamic worker pool - core implementation

   Workers send WorkerReady messages to owner, who then calls send_task to
   assign work.
*)
open Global
open Types

type 'task worker = 'task Types.worker

type 'task t = { coordinator_pid: Pid.t; task_ref: 'task Ref.t }

include PublicMessages

(** Start a worker pool in dynamic mode *)
let start: type task. concurrency:int -> owner:Pid.t -> worker_fn:(owner:Pid.t -> task:task -> unit) -> unit -> task t = fun ~concurrency ~owner ~worker_fn () ->
  let task_ref: task Ref.t = Ref.make () in
  let coordinator_pid =
    spawn
      (
        fun () -> Coordinator.init ~owner ~concurrency ~worker_fn ~task_ref
      )
  in
  { coordinator_pid; task_ref }

(** Send a task to a specific worker *)
let send_task: 'task t -> 'task worker -> 'task -> unit = fun t worker task ->
  let task = Task.make task t.task_ref in send worker.pid (ToWorker (Task task))

(** Get the task_ref from a worker for type equality checking *)
let get_worker_task_ref: 'task worker -> 'task Ref.t = fun worker -> worker.task_ref
