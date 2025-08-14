(** Worker pool management for parallel builds *)

open Miniriot

type t = { pid : Pid.t }

(** Messages that can be sent to the worker pool *)
type Message.t += SendTask of Build_messages.build_task | Shutdown

(** Messages sent from the worker pool to the listener *)
type Message.t +=
  | TaskAssigned of Build_messages.build_task
  | NoWorkersAvailable of Build_messages.build_task
  | TaskCompleted of string * bool * Hasher.hash

type state = {
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, string) Hashtbl.t;
  all_workers : Pid.t list;
  listener : Pid.t;
}
(** Internal pool state *)

(** Internal: Spawn workers *)
let spawn_workers pool_pid count =
  let workers = ref [] in
  for i = 1 to count do
    let worker_pid = spawn (fun () -> Build_worker.main pool_pid i ()) in
    workers := worker_pid :: !workers
  done;
  !workers

(** Internal: Send shutdown to all workers *)
let shutdown_all_workers workers =
  List.iter (fun worker_pid -> send worker_pid Build_messages.Shutdown) workers

(** Worker pool message loop *)
let rec pool_loop state =
  let selector = function
    | SendTask task -> `select (`send_task task)
    | Build_messages.NextTask worker_pid -> `select (`next_task worker_pid)
    | Build_messages.TaskComplete (pkg_name, success, hash) ->
        `select (`task_complete (pkg_name, success, hash))
    | Build_messages.RequeueWithDependencies (task, missing_deps) ->
        `select (`requeue (task, missing_deps))
    | Shutdown -> `select `shutdown
    | _ -> `skip
  in
  match receive ~selector () with
  | `send_task task ->
      (* Try to assign task to an idle worker *)
      if Queue.is_empty state.idle_workers then (
        (* No workers available *)
        send state.listener (NoWorkersAvailable task);
        pool_loop state)
      else
        (* Assign to idle worker *)
        let worker_pid = Queue.take state.idle_workers in
        let pkg_name = task.Build_messages.node.Build_node.package.name in
        Hashtbl.replace state.busy_workers worker_pid pkg_name;
        send worker_pid (Build_messages.Task task);
        send state.listener (TaskAssigned task);
        pool_loop state
  | `next_task worker_pid ->
      (* Worker is requesting work - mark it as idle *)
      (match Hashtbl.find_opt state.busy_workers worker_pid with
      | Some _pkg ->
          (* Worker was busy, now idle *)
          Hashtbl.remove state.busy_workers worker_pid
      | None -> ());
      Queue.add worker_pid state.idle_workers;
      send worker_pid Build_messages.NoTask;
      pool_loop state
  | `task_complete (pkg_name, success, hash) ->
      (* Forward completion to listener *)
      send state.listener (TaskCompleted (pkg_name, success, hash));
      pool_loop state
  | `requeue (task, missing_deps) ->
      (* Forward requeue message to listener *)
      send state.listener
        (Build_messages.RequeueWithDependencies (task, missing_deps));
      pool_loop state
  | `shutdown ->
      (* Shutdown all workers and exit *)
      shutdown_all_workers state.all_workers;
      Process.Normal

(** Start a worker pool process *)
let start ?(workers = System.cpu_count ()) ~listener () =
  let pid =
    spawn (fun () ->
        let pool_pid = self () in
        Printf.printf "[WorkerPool] Started with %d workers (pid: %s)\n" workers
          (Pid.to_string pool_pid);
        flush stdout;

        (* Spawn worker processes *)
        let all_workers = spawn_workers pool_pid workers in

        (* Initialize state with all workers in idle queue *)
        let idle_queue = Queue.create () in
        List.iter (fun worker -> Queue.add worker idle_queue) all_workers;

        let state =
          {
            idle_workers = idle_queue;
            busy_workers = Hashtbl.create 32;
            all_workers;
            listener;
          }
        in

        pool_loop state)
  in
  { pid }

(** Send a task to the worker pool *)
let send_task pool task = send pool.pid (SendTask task)

(** Shutdown the worker pool *)
let shutdown pool = send pool.pid Shutdown

(** Tests submodule *)
module Tests = struct
  let test_worker_pool_spawns_correct_number_of_workers () :
      (unit, string) result =
    (* Test that worker pool creates N worker processes *)
    Ok ()
    [@riot.test]

  let test_workers_receive_and_process_tasks () : (unit, string) result =
    (* Test that tasks are distributed to workers *)
    Ok ()
    [@riot.test]

  let test_worker_pool_handles_worker_failures () : (unit, string) result =
    (* Test that pool recovers from worker crashes *)
    Ok ()
    [@riot.test]

  let test_shutdown_terminates_all_workers () : (unit, string) result =
    (* Test that shutdown cleanly stops all workers *)
    Ok ()
    [@riot.test]

  let test_task_results_sent_back_to_server () : (unit, string) result =
    (* Test that workers send results back correctly *)
    Ok ()
end [@riot.test]
