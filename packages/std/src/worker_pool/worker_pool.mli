(** Generic worker pool for controlled parallel execution

    This module provides a worker pool pattern where a fixed number of worker
    processes execute tasks concurrently. This prevents resource exhaustion from
    spawning too many processes at once.

    Two modes of operation:

    1. Simple mode (start_with_tasks): Pre-queue all tasks upfront
    2. Advanced mode (start + WorkerReady): Dynamic task assignment

    Example - Simple mode:

    {[
      type my_task = string
      type Message.t += TaskResult of string * int

      let results = ref [] in
      let pool = Worker_pool.start_with_tasks ~workers:8
        ~owner:(self ())
        ~tasks:["task1"; "task2"; "task3"]
        ~worker_fn:(fun ~owner task ->
          let result = expensive_computation task in
          send owner (TaskResult (task, result)))
        () in

      (* Collect results as they arrive *)
      for _ = 1 to 3 do
        match receive_any () with
        | TaskResult (task, result) -> results := result :: !results
        | _ -> ()
      done;

      Worker_pool.stop pool
    ]}

    Example - Advanced mode:

    {[
      type Message.t +=
        | TaskResult of string * int
        | WorkerReady of Worker_pool.worker

      let pool = Worker_pool.start ~workers:8 ~owner:(self ())
        ~worker_fn:(fun ~owner task ->
          let result = expensive_computation task in
          send owner (TaskResult (task, result)))
        () in

      (* Assign tasks dynamically *)
      let rec dispatch_loop tasks =
        match receive_any () with
        | WorkerReady worker ->
            (match tasks with
            | task :: rest ->
                Worker_pool.send_task pool worker task;
                dispatch_loop rest
            | [] -> dispatch_loop [])
        | TaskResult _ -> dispatch_loop tasks
      in
      dispatch_loop ["task1"; "task2"; "task3"]
    ]} *)

open Global
open Miniriot

type 'task t
(** A pool of worker processes that execute tasks of type ['task]. *)

type 'task worker
(** An opaque handle to a worker that processes tasks of type ['task].
    Can only be used with [send_task]. Type safety prevents sending wrong task types. *)

(** {1 Simple Mode - Pre-queue Tasks} *)

val start_with_tasks :
  workers:int ->
  owner:Pid.t ->
  tasks:'task list ->
  worker_fn:(owner:Pid.t -> task:'task -> unit) ->
  unit ->
  'task t
(** [start_with_tasks ~workers ~owner ~tasks ~worker_fn ()] creates a worker
    pool and pre-queues all tasks. Workers automatically pull from the queue as
    they become ready.

    - [workers]: Number of concurrent worker processes to spawn
    - [owner]: The process that will receive messages from workers
    - [tasks]: List of all tasks to execute
    - [worker_fn]: Function executed by each worker for each task. The worker
      function receives the owner PID and the task, and is responsible for
      sending result messages back to the owner.

    This is the simplest mode - just provide all tasks upfront and collect
    results. The pool handles all scheduling automatically. *)

(** {1 Advanced Mode - Dynamic Task Assignment} *)

type Message.t += WorkerReady : 'task worker -> Message.t
(** Message sent to owner when a worker becomes ready for work. The owner must
    respond by calling [send_task] with a task for this worker.
    The worker is parameterized by task type for type safety. *)

val start :
  workers:int ->
  owner:Pid.t ->
  worker_fn:(owner:Pid.t -> task:'task -> unit) ->
  unit ->
  'task t
(** [start ~workers ~owner ~worker_fn ()] creates a worker pool with no
    pre-queued tasks. The owner will receive [WorkerReady worker] messages and
    must call [send_task pool worker task] to assign work.

    Use this mode when:
    - Tasks are generated dynamically based on results
    - Task assignment depends on external state
    - You need fine-grained control over scheduling *)

val send_task : 'task t -> 'task worker -> 'task -> unit
(** [send_task pool worker task] assigns a task to a specific worker.

    Only call this after receiving [WorkerReady worker] from the pool. Sending
    a task to a busy worker will queue it for that worker.

    Type safety: The worker and task must have matching types. *)

(** {1 Lifecycle} *)

val stop : 'task t -> unit
(** [stop pool] stops the worker pool and all worker processes.

    Note: This does not wait for in-flight tasks to complete. The caller should
    ensure all tasks have completed before calling stop. *)
