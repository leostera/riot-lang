open Std
open Std.Collections

type ('task, 'result, 'error) run_result = ('task * ('result, 'error) result) list

module DynamicWorkerPool = WorkerPool.DynamicWorkerPool

type ('task, 'result, 'error) task_result = {
  id: int;
  task: 'task;
  outcome: ('result, 'error) result;
  more: 'task list;
}

type Message.t +=
  | SchedulerTaskResult: {
      result: ('task, 'result, 'error) task_result;
      result_ref: ('task, 'result, 'error) task_result Ref.t;
    } -> Message.t

type ('task, 'result, 'error) state = {
  pool: (int * 'task) DynamicWorkerPool.t;
  task_queue: (int * 'task) Queue.t;
  idle_workers: (int * 'task) DynamicWorkerPool.worker Queue.t;
  result_ref: ('task, 'result, 'error) task_result Ref.t;
  mutable next_id: int;
  mutable tasks_in_flight: int;
  mutable results: (int * ('task * ('result, 'error) result)) list;
}

let enqueue_task = fun state task ->
  let id = state.next_id in
  state.next_id <- state.next_id + 1;
  Queue.push state.task_queue ~value:(id, task)

let enqueue_tasks = fun state tasks ->
  List.for_each tasks ~fn:(fun task -> enqueue_task state task)

let dispatch_available = fun state ->
  let rec loop () =
    match Queue.pop state.idle_workers, Queue.pop state.task_queue with
    | Some worker, Some task ->
        state.tasks_in_flight <- state.tasks_in_flight + 1;
        DynamicWorkerPool.send_task state.pool worker task;
        loop ()
    | Some worker, None ->
        Queue.push state.idle_workers ~value:worker
    | None, Some task ->
        Queue.push state.task_queue ~value:task
    | None, None -> ()
  in
  loop ()

let completed_results = fun state ->
  state.results
  |> List.sort ~compare:(fun (left, _) (right, _) -> Int.compare left right)
  |> List.map ~fn:(fun (_, item) -> item)

let is_complete = fun state ->
  state.tasks_in_flight = 0 && Queue.is_empty state.task_queue

let rec loop:
  type task result error.
  (task, result, error) state ->
  (task, result, error) run_result
  = fun state ->
  dispatch_available state;
  if is_complete state then
    completed_results state
  else
    let selector: ([
        `WorkerReady of (int * task) DynamicWorkerPool.worker
        | `TaskResult of (task, result, error) task_result
      ]) selector = fun msg ->
      match msg with
      | DynamicWorkerPool.WorkerReady worker -> (
          let worker_ref = DynamicWorkerPool.get_worker_task_ref worker in
          if Ref.equal state.pool.task_ref worker_ref then
            match Ref.type_equal state.pool.task_ref worker_ref with
            | Some Type.Equal -> `select (`WorkerReady worker)
            | None -> `skip
          else
            `skip
        )
      | SchedulerTaskResult { result; result_ref } -> (
          match Ref.type_equal state.result_ref result_ref with
          | Some Type.Equal -> `select (`TaskResult result)
          | None -> `skip
        )
      | _ -> `skip
    in
    match receive ~selector () with
    | `WorkerReady worker ->
        Queue.push state.idle_workers ~value:worker;
        loop state
    | `TaskResult result ->
        state.tasks_in_flight <- state.tasks_in_flight - 1;
        state.results <- (result.id, (result.task, result.outcome)) :: state.results;
        enqueue_tasks state result.more;
        loop state

let run:
  concurrency:int ->
  tasks:'task list ->
  fn:('task -> ('result * 'task list, 'error) result) ->
  ('task, 'result, 'error) run_result
  = fun ~concurrency ~tasks ~fn ->
  if tasks = [] then
    []
  else
    let concurrency = Int.max 1 concurrency in
    let result_ref = Ref.make () in
    let owner = self () in
    let worker_fn ~owner ~task:(id, task) =
      let result =
        match fn task with
        | Ok (result, more) -> { id; task; outcome = Ok result; more }
        | Error error -> { id; task; outcome = Error error; more = [] }
      in
      send owner (SchedulerTaskResult { result; result_ref })
    in
    let pool = DynamicWorkerPool.start ~concurrency ~owner ~worker_fn () in
    let state = {
      pool;
      task_queue = Queue.create ();
      idle_workers = Queue.create ();
      result_ref;
      next_id = 0;
      tasks_in_flight = 0;
      results = [];
    } in
    enqueue_tasks state tasks;
    loop state
