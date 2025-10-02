open Global
open Miniriot
open Types

type 'task state = {
  owner : Pid.t;
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, unit) Hashtbl.t;
  pending_tasks : ('task * 'task Ref.t) Queue.t;
  all_workers : Pid.t list;
  ref : 'task Ref.t;
}

(** Try to assign a task to an idle worker *)
let try_assign_task (state : 'task state) : unit =
  if
    (not (Queue.is_empty state.idle_workers))
    && not (Queue.is_empty state.pending_tasks)
  then (
    let worker_pid = Queue.pop state.idle_workers in
    let task, task_ref = Queue.pop state.pending_tasks in
    send worker_pid (ToWorker (Task (task, task_ref)));
    Hashtbl.add state.busy_workers worker_pid ())

(** Handle a worker becoming ready *)
let handle_worker_ready (state : 'task state) (worker_pid : Pid.t) : unit =
  (* Remove from busy workers *)
  Hashtbl.remove state.busy_workers worker_pid;

  (* Dynamic mode: notify owner and let them decide *)
  let worker_handle = { pid = worker_pid; ref = state.ref } in
  send state.owner (WorkerReady worker_handle);
  (* Mark as idle while waiting for owner to send task *)
  Queue.add worker_pid state.idle_workers

(** Coordinator loop - manages worker pool and task distribution *)
let rec loop (state : 'task state) : (unit, Process.exit_reason) result =
  let selector msg =
    match msg with
    | Types.FromWorker msg -> `select (`FromWorker msg)
    | Types.ToCoordinator msg -> `select (`ToCoordinator msg)
    | _ -> `skip
  in

  match receive ~selector () with
  | `FromWorker (TaskCompleted worker_pid) ->
      handle_worker_ready state worker_pid;
      loop state
  | `ToCoordinator (SendTask (task, task_ref)) ->
      (* Owner submitted a task (simple mode) *)
      Queue.add (task, task_ref) state.pending_tasks;
      try_assign_task state;
      loop state
  | `ToCoordinator (SendTaskToWorker (worker, task, task_ref)) ->
      (* Owner assigned task to specific worker (advanced mode) *)
      send worker.pid (ToWorker (Task (task, task_ref)));
      (* Mark worker as busy *)
      Hashtbl.remove state.busy_workers worker.pid;
      (* Remove from idle if it was there *)
      let new_idle = Queue.create () in
      Queue.iter
        (fun pid ->
          if not (Pid.equal pid worker.pid) then Queue.add pid new_idle)
        state.idle_workers;
      Queue.clear state.idle_workers;
      Queue.transfer new_idle state.idle_workers;
      Hashtbl.add state.busy_workers worker.pid ();
      loop state
  | `ToCoordinator Stop ->
      (* Shutdown all workers *)
      List.iter
        (fun worker_pid -> send worker_pid (ToWorker Stop))
        state.all_workers;
      Ok ()
