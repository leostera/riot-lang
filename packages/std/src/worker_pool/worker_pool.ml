(** Generic worker pool for controlled parallel execution *)

open Global
open Miniriot

type 'task worker = 'task Messages.worker
(** Re-export types from Messages *)

type 'task t = { coordinator_pid : Pid.t; ref : 'task Ref.t }
(** Pool handle *)

module SimpleWorkerPool = struct
  type 'task t = 'task t

  (** Start a worker pool with pre-queued tasks (Simple mode) *)
  let start_with_tasks : type task.
      workers:int ->
      owner:Pid.t ->
      tasks:task list ->
      worker_fn:(owner:Pid.t -> task:task -> unit) ->
      unit ->
      task t =
   fun ~workers ~owner ~tasks ~worker_fn () ->
    let ref : task Ref.t = Ref.make () in
    let coordinator_pid =
      spawn (fun () ->
          let coordinator = self () in

          (* Spawn N workers *)
          let worker_pids =
            List.init workers (fun _ ->
                let worker_state =
                  Worker.{ coordinator; owner; worker_fn; ref }
                in
                spawn (fun () -> Worker.loop worker_state))
          in

          (* Create coordinator state in Simple mode *)
          let state =
            Coordinator.
              {
                mode = Simple;
                owner;
                idle_workers = Queue.create ();
                busy_workers = Hashtbl.create workers;
                pending_tasks = Queue.create ();
                all_workers = worker_pids;
                ref;
              }
          in

          (* Pre-queue all tasks *)
          List.iter
            (fun task -> Queue.add (task, ref) state.pending_tasks)
            tasks;

          (* All workers start idle *)
          List.iter (fun pid -> Queue.add pid state.idle_workers) worker_pids;

          (* Start assigning tasks to workers *)
          for _ = 1 to min workers (List.length tasks) do
            Coordinator.try_assign_task state
          done;

          Coordinator.loop state)
    in
    { coordinator_pid; ref }
end

module DynamicWorkerPool = struct
  type 'task t = 'task t
  type 'task worker = 'task worker

  type Message.t += WorkerReady : 'task worker -> Message.t

  (** Start a worker pool in dynamic mode (Advanced mode) *)
  let start : type task.
      workers:int ->
      owner:Pid.t ->
      worker_fn:(owner:Pid.t -> task:task -> unit) ->
      unit ->
      task t =
   fun ~workers ~owner ~worker_fn () ->
    let ref : task Ref.t = Ref.make () in
    let coordinator_pid =
      spawn (fun () ->
          let coordinator = self () in

          (* Spawn N workers *)
          let worker_pids =
            List.init workers (fun _ ->
                let worker_state =
                  Worker.{ coordinator; owner; worker_fn; ref }
                in
                spawn (fun () -> Worker.loop worker_state))
          in

          (* Create coordinator state in Advanced mode *)
          let state =
            Coordinator.
              {
                mode = Advanced;
                owner;
                idle_workers = Queue.create ();
                busy_workers = Hashtbl.create workers;
                pending_tasks = Queue.create ();
                all_workers = worker_pids;
                ref;
              }
          in

          (* All workers start idle - send WorkerReady for each *)
          List.iter
            (fun pid ->
              let worker_handle = Messages.{ pid; ref } in
              send owner (Messages.WorkerReady worker_handle))
            worker_pids;

          (* Mark all as idle *)
          List.iter (fun pid -> Queue.add pid state.idle_workers) worker_pids;

          Coordinator.loop state)
    in
    { coordinator_pid; ref }

  (** Send a task to a specific worker *)
  let send_task (t : 'task t) (worker : 'task worker) (task : 'task) : unit =
    (* Verify type safety *)
    match Ref.type_equal t.ref worker.ref with
    | Some Type.Equal ->
        send t.coordinator_pid
          (Messages.ToCoordinator
             (Messages.SendTaskToWorker (worker, task, t.ref)))
    | None -> panic "send_task: worker and pool have mismatched task types"

  (** Stop the pool and all workers *)
  let stop (t : 'task t) : unit =
    send t.coordinator_pid (Messages.ToCoordinator Messages.Stop)
end
