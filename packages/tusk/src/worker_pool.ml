(** Worker pool management for parallel builds *)

open Miniriot

type task = {
  node : Build_node.t;
  workspace : Workspace.t;
  session_id : Log.session_id option;
}
(** Build task with context *)

(** Worker pool messages *)
type Message.t +=
  | Worker of Message.t (* Wrapper for worker messages *)
  | WorkerReady of Pid.t
  | Task of task
  | TaskCompleted of {
      worker : Pid.t;
      node : Build_node.t;
      artifact : Store.artifact;
    }
  | TaskFailed of { worker : Pid.t; node : Build_node.t; error : string }
  | RequeueWithDependencies of {
      worker : Pid.t;
      node : Build_node.t;
      deps : Build_node.t list;
    }

type t = { pid : Pid.t }

type state = {
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, string) Hashtbl.t;
  all_workers : Pid.t list;
  worker_ids : (Pid.t, Worker_id.t) Hashtbl.t;
  provider : Pid.t; (* The process that provides tasks (build server) *)
}

(** Send a task to a specific worker *)
let send_task worker task = send worker (Task task)

(** Internal: Spawn workers *)
let spawn_workers pool_pid count worker_fn =
  let workers = ref [] in
  let worker_id_map = Hashtbl.create count in
  for i = 1 to count do
    let worker_pid = spawn (worker_fn pool_pid (Worker_id.make i)) in
    workers := worker_pid :: !workers;
    Hashtbl.add worker_id_map worker_pid (Worker_id.make i)
  done;
  (!workers, worker_id_map)

(** Start a worker pool *)
let start ~workers ~provider ~worker_fn () =
  let pool_pid =
    spawn (fun () ->
        let all_workers, worker_ids =
          spawn_workers (self ()) workers worker_fn
        in
        let state =
          {
            idle_workers = Queue.create ();
            busy_workers = Hashtbl.create workers;
            all_workers;
            worker_ids;
            provider;
          }
        in

        (* All workers start as idle *)
        List.iter (fun w -> Queue.add w state.idle_workers) all_workers;

        let rec pool_loop state =
          (* Wait for messages from workers or provider *)
          match receive_any () with
          | Worker (WorkerReady worker_pid) ->
              (* Worker is ready for work *)
              (* Remove from busy if it was there *)
              Hashtbl.remove state.busy_workers worker_pid;
              (* Add to idle queue *)
              Queue.add worker_pid state.idle_workers;
              (* Forward the WorkerReady message to the provider *)
              send state.provider (Worker (WorkerReady worker_pid));
              pool_loop state
          | Worker (TaskCompleted { worker; node; artifact }) ->
              (* Worker completed a task *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider
                (Worker (TaskCompleted { worker; node; artifact }));
              pool_loop state
          | Worker (TaskFailed { worker; node; error }) ->
              (* Worker failed a task *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider (Worker (TaskFailed { worker; node; error }));
              pool_loop state
          | Worker (RequeueWithDependencies { worker; node; deps }) ->
              (* Worker needs dependencies *)
              Hashtbl.remove state.busy_workers worker;
              Queue.add worker state.idle_workers;
              (* Forward to provider *)
              send state.provider
                (Worker (RequeueWithDependencies { worker; node; deps }));
              pool_loop state
          | _ -> pool_loop state
        in

        pool_loop state)
  in

  { pid = pool_pid }
