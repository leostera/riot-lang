(** Worker pool management for parallel builds *)

open Miniriot

type t = { pid : Pid.t }

type state = {
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, string) Hashtbl.t;
  all_workers : Pid.t list;
  provider : Pid.t; (* The process that provides tasks (build server) *)
}

(** Send a task to a specific worker *)
let send_task worker task = send worker (Worker_pool_types.Task task)

(** Internal: Spawn workers *)
let spawn_workers count worker_fn context =
  let workers = ref [] in
  for i = 1 to count do
    let worker_pid = spawn (worker_fn context) in
    workers := worker_pid :: !workers
  done;
  !workers

(** Start a worker pool *)
let start ~workers ~provider ~build_graph ~build_results ~workspace ~store
    ~worker_fn () =
  let pool_pid =
    spawn (fun () ->
        (* Create the context with the pool's PID as server_pid *)
        let context =
          Worker_pool_types.{ server_pid = self (); build_graph; build_results; workspace; store }
        in
        let all_workers = spawn_workers workers worker_fn context in
        let state =
          {
            idle_workers = Queue.create ();
            busy_workers = Hashtbl.create workers;
            all_workers;
            provider;
          }
        in

        (* All workers start as idle *)
        List.iter (fun w -> Queue.add w state.idle_workers) all_workers;

        let rec pool_loop state =
          (* Wait for messages from workers or provider *)
          match receive_any () with
          | Worker_pool_types.Worker (Worker_pool_types.WorkerReady worker_pid) ->
              (* Worker is ready for work *)
              (* Remove from busy if it was there *)
              Hashtbl.remove state.busy_workers worker_pid;
              (* Add to idle queue *)
              Queue.add worker_pid state.idle_workers;
              (* Forward the WorkerReady message to the provider *)
              send state.provider (Worker_pool_types.Worker (Worker_pool_types.WorkerReady worker_pid));
              pool_loop state
          | Worker_pool_types.Worker (Worker_pool_types.TaskCompleted { worker; node; artifact }) ->
              (* Worker completed a task *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider
                (Worker_pool_types.Worker (Worker_pool_types.TaskCompleted { worker; node; artifact }));
              pool_loop state
          | Worker_pool_types.Worker (Worker_pool_types.TaskFailed { worker; node; error }) ->
              (* Worker failed a task *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider (Worker_pool_types.Worker (Worker_pool_types.TaskFailed { worker; node; error }));
              pool_loop state
          | Worker_pool_types.Worker (Worker_pool_types.RequeueWithDependencies { worker; node; deps }) ->
              (* Worker needs dependencies *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider
                (Worker_pool_types.Worker (Worker_pool_types.RequeueWithDependencies { worker; node; deps }));
              pool_loop state
          | _ -> pool_loop state
        in

        pool_loop state)
  in

  { pid = pool_pid }
