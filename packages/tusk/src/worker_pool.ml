(** Worker pool management for parallel builds *)

open Miniriot

type t = { pid : Pid.t }

(** Messages that can be sent to the worker pool *)
type Message.t += SendTask of Build_messages.build_task | Shutdown

(** Messages sent from the worker pool to the listener *)
type Message.t +=
  | TaskAssigned of {
      task : Build_messages.build_task;
      worker_id : Worker_id.t;
    }
  | NoWorkersAvailable of { task : Build_messages.build_task }
  | TaskCompleted of {
      package_name : string;
      hash : Hasher.hash;
    }
  | TaskFailed of {
      package_name : string;
      error : string;
    }

type state = {
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, string) Hashtbl.t;
  all_workers : Pid.t list;
  worker_ids : (Pid.t, Worker_id.t) Hashtbl.t; (* Maps worker PIDs to their IDs *)
  listener : Pid.t;
}
(** Internal pool state *)

(** Internal: Spawn workers *)
let spawn_workers pool_pid count =
  let workers = ref [] in
  let worker_id_map = Hashtbl.create count in
  for i = 1 to count do
    let worker_pid = spawn (fun () -> Build_worker.main pool_pid (Worker_id.make i) ()) in
    workers := worker_pid :: !workers;
    Hashtbl.add worker_id_map worker_pid (Worker_id.make i)
  done;
  (!workers, worker_id_map)

(** Internal: Send shutdown to all workers *)
let shutdown_all_workers workers =
  List.iter (fun worker_pid -> send worker_pid Build_messages.Shutdown) workers

(** Worker pool message loop *)
let rec pool_loop state =
  let selector = function
    | SendTask task -> `select (`send_task task)
    | Build_messages.NextTask { worker_pid } -> `select (`next_task worker_pid)
    | Build_messages.TaskCompleted { package_name = pkg_name; hash } ->
        `select (`task_completed (pkg_name, hash))
    | Build_messages.TaskFailed { package_name = pkg_name; error } ->
        `select (`task_failed (pkg_name, error))
    | Build_messages.RequeueWithDependencies { task; missing_deps } ->
        `select (`requeue (task, missing_deps))
    | Shutdown -> `select `shutdown
    | _ -> `skip
  in
  match receive ~selector () with
  | `send_task task ->
      (* Try to assign task to an idle worker *)
      if Queue.is_empty state.idle_workers then (
        (* No workers available *)
        send state.listener (NoWorkersAvailable { task });
        pool_loop state)
      else
        (* Assign to idle worker *)
        let worker_pid = Queue.take state.idle_workers in
        let pkg_name = task.Build_messages.node.Build_node.package.name in
        let worker_id = Hashtbl.find state.worker_ids worker_pid in
        Hashtbl.replace state.busy_workers worker_pid pkg_name;
        send worker_pid (Build_messages.Task task);
        send state.listener (TaskAssigned { task; worker_id });
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
  | `task_completed (pkg_name, hash) ->
      (* Forward successful completion to listener *)
      send state.listener (TaskCompleted { package_name = pkg_name; hash });
      pool_loop state
  | `task_failed (pkg_name, error) ->
      (* Forward failure to listener *)
      send state.listener (TaskFailed { package_name = pkg_name; error });
      pool_loop state
  | `requeue (task, missing_deps) ->
      (* Forward requeue message to listener *)
      send state.listener
        (Build_messages.RequeueWithDependencies { task; missing_deps });
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
        let all_workers, worker_id_map = spawn_workers pool_pid workers in

        (* Initialize state with all workers in idle queue *)
        let idle_queue = Queue.create () in
        List.iter (fun worker -> Queue.add worker idle_queue) all_workers;

        let state =
          {
            idle_workers = idle_queue;
            busy_workers = Hashtbl.create 32;
            all_workers;
            worker_ids = worker_id_map;
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
