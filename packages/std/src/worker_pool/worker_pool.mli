(**
   Controlled parallel execution.

   A worker pool pattern where a fixed number of worker processes execute tasks
   concurrently. This prevents resource exhaustion from spawning too many
   processes at once.

   ## Two Modes of Operation

   ### 1. Simple Mode - Parallel Map

   Use [SimpleWorkerPool.run] for straightforward parallel processing:

   ```ocaml open Std

   (* Process files in parallel with 8 workers *) let files =
   ["file1.txt"; "file2.txt"; "file3.txt"] in let results =
   WorkerPool.SimpleWorkerPool.run ~concurrency:8 ~tasks:files ~fn:(fun file ->
   let content = Fs.read (Path.v file) |> Result.unwrap in String.length
   content ) () in

   (* results: [(0, 100); (1, 250); (2, 180)] - ordered by task index *) ```

   ### 2. Dynamic Mode - Manual Task Assignment

   Use [DynamicWorkerPool] when tasks are generated dynamically or depend on
   previous results:

   ```ocaml

   type Message.t += TaskResult of string * int

   let pool = WorkerPool.DynamicWorkerPool.start ~concurrency:8 ~owner:(self
   ()) ~worker_fn:(fun ~owner ~task -> let result = expensive_computation task
   in send owner (TaskResult (task, result)) ) () in

   (* Dynamically assign tasks as workers become ready *) let rec dispatch_loop
   remaining_tasks = match receive_any () with |
   WorkerPool.DynamicWorkerPool.WorkerReady worker -> (match remaining_tasks
   with | task :: rest -> WorkerPool.DynamicWorkerPool.send_task pool worker
   task; dispatch_loop rest | [] -> Log.info "All tasks dispatched";
   collect_results ()) | TaskResult (task, result) -> Log.info "Task %s
   completed: %d" task result; dispatch_loop remaining_tasks in dispatch_loop
   ["task1"; "task2"; "task3"] ```

   ## When to Use WorkerPool

   - CPU-bound parallel tasks with controlled concurrency
   - Processing large collections in parallel
   - Preventing resource exhaustion from too many processes
   - Implementing map-reduce patterns

   ## When to Use Alternatives

   - **I/O-bound tasks**: Use [Task.async] for lightweight async I/O
   - **Single task**: Just use [spawn] directly
   - **Unbounded parallelism OK**: Use [Task.async] or spawn manually

   ## Performance Characteristics

   - Task distribution: O(1) per task
   - Overhead: Fixed pool of N workers, minimal scheduling overhead
   - Memory: O(N) for N workers + task queue
*)
open Global

module DynamicWorkerPool: sig
  (**
     A pool of worker processes that execute tasks of type ['task]. The
     [task_ref] field is a phantom type witness used for type-safe pattern
     matching on [WorkerReady] messages.
  *)

  (**
     An opaque handle to a worker that processes tasks of type ['task]. Can
     only be used with [send_task]. Type safety prevents sending wrong task
     types.
  *)
  type 'task t = {
    coordinator_pid: Pid.t;
    task_ref: 'task Ref.t;
  }
  (** Get the task_ref from a worker for type equality checking. *)
  type 'task worker

  val get_worker_task_ref: 'task worker -> 'task Ref.t

  (** {1 Advanced Mode - Dynamic Task Assignment} *)

  (**
     Message sent to owner when a worker becomes ready for work. The
     owner must respond by calling [send_task] with a task for this
     worker. The worker is parameterized by task type for type safety.
  *)
  type Message.t +=
    | WorkerReady: 'task worker -> Message.t

  (**
     [start ~concurrency ~owner ~worker_fn ()] creates a worker pool with no
     pre-queued tasks. The owner will receive [WorkerReady worker] messages and
     must call [send_task pool worker task] to assign work.

     Use this mode when:
     - Tasks are generated dynamically based on results
     - Task assignment depends on external state
     - You need fine-grained control over scheduling
  *)
  val start:
    concurrency:int ->
    owner:Pid.t ->
    worker_fn:(owner:Pid.t -> task:'task -> unit) ->
    unit ->
    'task t

  val send_task: 'task t -> 'task worker -> 'task -> unit

  (**
     [send_task pool worker task] assigns a task to a specific worker.

     Only call this after receiving [WorkerReady worker] from the pool. Sending
     a task to a busy worker will queue it for that worker.

     Type safety: The worker and task must have matching types.
  *)
  (** {1 Lifecycle} *)
end

module SimpleWorkerPool: sig
  (** {1 Simple Mode - Parallel Map} *)

  val run:
    ?concurrency:int ->
    tasks:'task list ->
    fn:('task -> 'result) ->
    unit ->
    (int * 'result) list

  (**
     [run ~concurrency ~tasks ~fn ()] executes [fn] on each task in parallel
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
     - Raises: Any exception raised by [fn], re-raised in the caller

     The operation blocks until all tasks complete. Workers automatically pull
     from the task queue as they become ready.
  *)
end
