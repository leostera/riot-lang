open Global

open Types

type 'task state = {
  owner : Pid.t;
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, unit) Hashtbl.t;
  pending_tasks : Task.t Queue.t;
  all_workers : Pid.t list;
  task_ref : 'task Ref.t;
}

let rec loop : type task. task state -> (unit, Process.exit_reason) result =
 fun state ->
  let selector msg =
    match msg with ToCoordinator msg -> `select msg | _ -> `skip
  in

  match receive ~selector () with
  | WorkerReady worker -> (
      match Ref.type_equal state.task_ref worker.task_ref with
      | Some Type.Equal -> handle_worker_ready state worker
      | None -> panic "Received worker of the wrong type?!")

and handle_worker_ready : type task.
    task state -> task worker -> (unit, Process.exit_reason) result =
 fun state worker ->
  Hashtbl.remove state.busy_workers worker.pid;
  send state.owner PublicMessages.(WorkerReady worker);
  Queue.add worker.pid state.idle_workers;
  loop state

let init ~owner ~concurrency ~worker_fn ~task_ref =
  let coordinator = self () in

  (* Spawn N workers *)
  let worker_pids =
    let rec spawn_n acc n =
      if n = 0 then List.rev acc
      else
        let pid = Worker.start ~coordinator ~owner ~worker_fn ~task_ref in
        spawn_n (pid :: acc) (n - 1)
    in
    spawn_n [] concurrency
  in

  let worker_handles = List.map (fun pid -> { pid; task_ref }) worker_pids in

  (* Create coordinator state *)
  let state =
    {
      owner;
      idle_workers = Queue.create ();
      busy_workers = Hashtbl.create concurrency;
      pending_tasks = Queue.create ();
      all_workers = worker_pids;
      task_ref;
    }
  in

  (* Mark all as idle *)
  List.iter (fun pid -> Queue.add pid state.idle_workers) worker_pids;

  (* All workers start idle - send WorkerReady for each *)
  List.iter
    (fun handle -> send coordinator (ToCoordinator (WorkerReady handle)))
    worker_handles;

  loop state
