(** Generic worker pool for controlled parallel execution

    This module provides a worker pool pattern where a fixed number of worker
    processes execute tasks concurrently. This prevents resource exhaustion from
    spawning too many processes at once.

    Two modes of operation:

    1. Simple mode (run): Parallel map with controlled concurrency 2. Dynamic
    mode (start + WorkerReady): Manual task assignment

    Example - Simple mode (parallel map):

    {[
      (* Run tasks in parallel with 8 workers *)
      let results =
        Worker_pool.SimpleWorkerPool.run ~concurrency:8
          ~tasks:[ "task1"; "task2"; "task3" ]
          ~fn:(fun task -> expensive_computation task)
          ()

      (* results is a list in the same order as tasks *)
    ]}

    Example - Dynamic mode:

    {[
      type Message.t +=
        | TaskResult of string * int

      let pool = Worker_pool.DynamicWorkerPool.start ~concurrency:8 ~owner:(self ())
        ~worker_fn:(fun ~owner ~task ->
          let result = expensive_computation task in
          send owner (TaskResult (task, result)))
        () in

      (* Assign tasks dynamically *)
      let rec dispatch_loop tasks =
        match receive_any () with
        | Worker_pool.DynamicWorkerPool.WorkerReady worker ->
            (match tasks with
            | task :: rest ->
                Worker_pool.DynamicWorkerPool.send_task pool worker task;
                dispatch_loop rest
            | [] -> dispatch_loop [])
        | TaskResult _ -> dispatch_loop tasks
      in
      dispatch_loop ["task1"; "task2"; "task3"]
    ]} *)

open Global
open Miniriot

module DynamicWorkerPool : sig
  type 'task t
  (** A pool of worker processes that execute tasks of type ['task]. *)

  type 'task worker
  (** An opaque handle to a worker that processes tasks of type ['task]. Can
      only be used with [send_task]. Type safety prevents sending wrong task
      types. *)

  (** {1 Advanced Mode - Dynamic Task Assignment} *)

  type Message.t +=
    | WorkerReady : 'task worker -> Message.t
          (** Message sent to owner when a worker becomes ready for work. The
              owner must respond by calling [send_task] with a task for this
              worker. The worker is parameterized by task type for type safety.
          *)

  val start :
    concurrency:int ->
    owner:Pid.t ->
    worker_fn:(owner:Pid.t -> task:'task -> unit) ->
    unit ->
    'task t
  (** [start ~concurrency ~owner ~worker_fn ()] creates a worker pool with no
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
end

module SimpleWorkerPool : sig
  (** {1 Simple Mode - Parallel Map} *)

  val run :
    ?concurrency:int ->
    tasks:'task list ->
    fn:('task -> 'result) ->
    unit ->
    (int * 'result) list
  (** [run ~concurrency ~tasks ~fn ()] executes [fn] on each task in parallel
      using a pool of worker processes, collecting results in order.

      This is like [List.map] but with controlled parallelism:
      {[
        (* Sequential *)
        let results = List.map (fun x -> expensive_computation x) tasks

        (* Parallel with 8 workers *)
        let results =
          SimpleWorkerPool.run ~concurrency:8 ~tasks
            ~fn:(fun x -> expensive_computation x)
            ()
      ]}

      - [concurrency]: Number of concurrent workers (default: 8)
      - [tasks]: List of tasks to execute
      - [fn]: Function to execute on each task
      - Returns: Results in the same order as input tasks

      The operation blocks until all tasks complete. Workers automatically pull
      from the task queue as they become ready. *)
end
