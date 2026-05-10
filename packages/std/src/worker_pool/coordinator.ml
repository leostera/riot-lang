open Global
open Collections
open Types

type 'task state = {
  owner: Pid.t;
  idle_workers: Pid.t Queue.t;
  busy_workers: (Pid.t, unit) HashMap.t;
  pending_tasks: Task.t Queue.t;
  all_workers: Pid.t list;
  task_ref: 'task Ref.t;
}

let rec loop: type task. task state -> (unit, Actor.exit_reason) result = fun state ->
  let selector msg =
    match msg with
    | ToCoordinator msg -> Select msg
    | _ -> Skip
  in
  match receive ~selector () with
  | WorkerReady worker ->
      match Ref.type_equal state.task_ref worker.task_ref with
      | Some Type.Equal -> handle_worker_ready state worker
      | None -> panic "Received worker of the wrong type?!"

and handle_worker_ready: type task. task state -> task worker -> (unit, Actor.exit_reason) result = fun
  state worker ->
  let _ = HashMap.remove state.busy_workers ~key:worker.pid in
  send state.owner PublicMessages.(WorkerReady worker);
  Queue.push state.idle_workers ~value:worker.pid;
  loop state

let init = fun ~owner ~concurrency ~worker_fn ~task_ref ->
  let coordinator = self () in
  (* Spawn N workers *)
  let worker_pids =
    let rec spawn_n acc n =
      if n = 0 then
        List.reverse acc
      else
        let pid = Worker.start ~coordinator ~owner ~worker_fn ~task_ref in
        spawn_n (pid :: acc) (n - 1)
    in
    spawn_n [] concurrency
  in
  let worker_handles = List.map worker_pids ~fn:(fun pid -> { pid; task_ref }) in
  (* Create coordinator state *)
  let state = {
    owner;
    idle_workers = Queue.create ();
    busy_workers = HashMap.create ();
    pending_tasks = Queue.create ();
    all_workers = worker_pids;
    task_ref;
  }
  in
  (* Mark all as idle *)
  List.for_each worker_pids ~fn:(fun pid -> Queue.push state.idle_workers ~value:pid);
  (* All workers start idle - send WorkerReady for each *)
  List.for_each
    worker_handles
    ~fn:(fun handle -> send coordinator (ToCoordinator (WorkerReady handle)));
  loop state
