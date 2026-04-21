(** Simple worker pool - parallel map with controlled concurrency

    Provides a simple API for parallel execution: like List.map but with limited
    workers. *)
open Global
open Collections
open Types

type ('task, 'result) state = {
  owner: Pid.t;
  pool: 'task Dynamic.t;
  task_queue: 'task Queue.t;
  mutable results: (int * 'result) list;
  result_ref: 'result Ref.t;
  mutable tasks_in_flight: int;
}

type Message.t +=
  | TaskResult: {
      idx: int;
      result: 'result;
      result_ref: 'result Ref.t;
    } -> Message.t
  | TaskError: {
      idx: int;
      exn: exn;
      result_ref: 'result Ref.t;
    } -> Message.t
  | Completed: {
      results: (int * 'result) list;
      result_ref: 'result Ref.t;
    } -> Message.t
  | Failed: {
      idx: int;
      exn: exn;
      result_ref: 'result Ref.t;
    } -> Message.t

let rec loop: type task res. (task, res) state -> (unit, Actor.exit_reason) result = fun state ->
  let selector: ([
      `WorkerReady of task worker
      | `TaskResult of int * res
      | `TaskError of int * exn
    ]) selector = fun msg ->
    match msg with
    | Dynamic.WorkerReady worker -> (
        match Ref.type_equal state.pool.task_ref worker.task_ref with
        | Some Type.Equal -> `select (`WorkerReady worker)
        | None -> panic "bad message"
      )
    | TaskResult { idx; result; result_ref } -> (
        match Ref.type_equal state.result_ref result_ref with
        | Some Type.Equal -> `select (`TaskResult (idx, result))
        | None -> panic "bad message"
      )
    | TaskError { idx; exn; result_ref } -> (
        match Ref.type_equal state.result_ref result_ref with
        | Some Type.Equal -> `select (`TaskError (idx, exn))
        | None -> panic "bad message"
      )
    | _ ->
        `skip
  in
  match receive ~selector () with
  | `TaskResult res ->
      state.results <- res :: state.results;
      state.tasks_in_flight <- state.tasks_in_flight - 1;
      loop state
  | `TaskError (idx, exn) ->
      send state.owner (Failed { idx; exn; result_ref = state.result_ref });
      Ok ()
  | `WorkerReady worker -> (
      match Queue.pop state.task_queue with
      | Some task ->
          state.tasks_in_flight <- state.tasks_in_flight + 1;
          Dynamic.send_task state.pool worker task;
          loop state
      | None ->
          (* Only send Completed when queue is empty AND no tasks in flight *)
          if state.tasks_in_flight = 0 then
            (
              let results =
                List.sort state.results
                  ~compare:(fun (left_idx, _) (right_idx, _) ->
                    Int.compare left_idx right_idx)
              in
              send state.owner (Completed { results; result_ref = state.result_ref });
              Ok ()
            )
          else
            loop state
    )

let init = fun ~owner ~concurrency ~tasks ~result_ref ~fn () ->
  let dispatcher_self = self () in
  (* Worker function: execute user's fn and send result back *)
  let worker_fn ~owner ~task:(idx, task) =
    match fn task with
    | result -> send owner (TaskResult { idx; result; result_ref })
    | exception exn -> send owner (TaskError { idx; exn; result_ref })
  in
  (* Start dynamic pool with indexed tasks *)
  let indexed_tasks, _ =
    List.fold_left tasks ~acc:([], 0) ~fn:(fun (acc, idx) task -> ((idx, task) :: acc, idx + 1))
  in
  let indexed_tasks = List.reverse indexed_tasks in
  let task_queue = Queue.create () in
  List.for_each indexed_tasks ~fn:(fun t -> Queue.push task_queue ~value:t);
  let results = [] in
  let pool = Dynamic.start ~concurrency ~owner:dispatcher_self ~worker_fn () in
  loop
    {
      owner;
      pool;
      task_queue;
      results;
      result_ref;
      tasks_in_flight = 0;
    }

(** Run tasks in parallel with limited concurrency, collecting results in order
*)
let run:
  type task result. ?concurrency:int ->
  tasks:task list ->
  fn:(task -> result) ->
  unit ->
  (int * result) list = fun ?(concurrency = Thread.available_parallelism) ~tasks ~fn () ->
  (* Edge case: empty task list *)
  if tasks = [] then
    []
  else
    let result_ref: result Ref.t = Ref.make () in
    let owner = self () in
    (* Spawn dispatcher process *)
    let _dispatcher_pid = spawn (init ~owner ~result_ref ~concurrency ~tasks ~fn) in
    let selector: ([
        `Completed of (int * result) list
        | `Failed of exn
      ]) selector = function
      | Completed { results; result_ref=ref } when Ref.equal result_ref ref -> (
          match Ref.type_equal result_ref ref with
          | Some Type.Equal -> `select (`Completed results)
          | None -> panic "bad message"
        )
      | Failed { exn; result_ref=ref; _ } when Ref.equal result_ref ref -> (
          match Ref.type_equal result_ref ref with
          | Some Type.Equal -> `select (`Failed exn)
          | None -> panic "bad message"
        )
      | _ ->
          `skip
    in
    match receive ~selector () with
    | `Completed results -> results
    | `Failed exn -> raise exn
