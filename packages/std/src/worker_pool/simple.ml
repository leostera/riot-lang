(** Simple worker pool - convenience wrapper around Dynamic

    Pre-queues all tasks and automatically dispatches them to workers.
    Workers still send results directly to the actual owner (no dispatcher hop). *)

open Global
open Miniriot
open Types

type 'task t = {
  dispatcher_pid : Pid.t;
  pool : 'task Dynamic.t;
  actual_owner : Pid.t;
}

(** Start a worker pool with pre-queued tasks *)
let start_with_tasks :
    type task.
    workers:int ->
    owner:Pid.t ->
    tasks:task list ->
    worker_fn:(owner:Pid.t -> task:task -> unit) ->
    unit ->
    task t =
 fun ~workers ~owner ~tasks ~worker_fn () ->
  (* Create a task queue *)
  let task_queue = Queue.create () in
  List.iter (fun task -> Queue.add task task_queue) tasks;
  let remaining = ref (List.length tasks) in

  (* Spawn dispatcher process *)
  let dispatcher_pid =
    spawn (fun () ->
        let dispatcher_self = self () in

        (* Start dynamic pool with dispatcher as the owner *)
        let pool =
          Dynamic.start ~workers ~owner:dispatcher_self ~worker_fn ()
        in

        (* Dispatch loop: receive WorkerReady, send next task *)
        let rec dispatch_loop () =
          match receive_any () with
          | Dynamic.WorkerReady worker ->
              if !remaining > 0 && not (Queue.is_empty task_queue) then (
                let task = Queue.pop task_queue in
                Dynamic.send_task pool worker task;
                decr remaining
              );
              dispatch_loop ()
          | ToCoordinator Stop ->
              (* Forward stop to underlying pool *)
              Dynamic.stop pool;
              Ok ()
          | _ -> dispatch_loop ()
        in

        dispatch_loop ())
  in

  (* We need a placeholder pool handle for the public API *)
  (* The actual pool is inside the dispatcher, but we return a handle *)
  let dummy_ref = Ref.make () in
  let dummy_pool = Dynamic.{ coordinator_pid = dispatcher_pid; ref = dummy_ref } in

  { dispatcher_pid; pool = dummy_pool; actual_owner = owner }
