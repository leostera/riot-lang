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
  | Completed: {
      results: (int * 'result) list;
      result_ref: 'result Ref.t;
    } -> Message.t

let rec loop : type task res. (task, res) state -> (unit, Process.exit_reason) result = fun state ->
    let selector : ([
      `WorkerReady of task worker
      | `TaskResult of int * res
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
        | _ ->
            `skip
    in
    match receive ~selector () with
    | `TaskResult res ->
        state.results <- res :: state.results;
        state.tasks_in_flight <- state.tasks_in_flight - 1;
        loop state
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
                send state.owner (Completed {results = state.results; result_ref = state.result_ref});
                Ok ()
              )
            else
              loop state
      )

let init = fun ~owner ~concurrency ~tasks ~result_ref ~fn () ->
    let dispatcher_self = self () in
    (* Worker function: execute user's fn and send result back *)
    let worker_fn ~owner ~task:(idx, task) =
      let result = fn task in
      send owner (TaskResult {idx; result; result_ref})
    in
    (* Start dynamic pool with indexed tasks *)
    let indexed_tasks =
      List.mapi (fun idx task -> (idx, task)) tasks
    in
    let task_queue = Queue.create () in
    List.iter
      (fun t ->
        Queue.push task_queue t)
      indexed_tasks;
    let results = [] in
    let pool = Dynamic.start ~concurrency ~owner:dispatcher_self ~worker_fn () in
    loop {owner; pool; task_queue; results; result_ref; tasks_in_flight = 0}

(** Run tasks in parallel with limited concurrency, collecting results in order
*)
let run : type task result. ?concurrency:int ->
tasks:task list ->
fn:(task -> result) ->
unit ->
(int * result) list = fun ?(concurrency = System.available_parallelism) ~tasks ~fn () ->
    (* Edge case: empty task list *)
    if tasks = [] then
      []
    else
      let result_ref : result Ref.t = Ref.make () in
      let owner = self () in
      (* Spawn dispatcher process *)
      let _dispatcher_pid = spawn (init ~owner ~result_ref ~concurrency ~tasks ~fn) in
      let selector : (int * result) list selector = function
        | Completed { results; result_ref=ref } when Ref.equal result_ref ref -> (
            match Ref.type_equal result_ref ref with
            | Some Type.Equal -> `select results
            | None -> panic "bad message"
          )
        | _ -> `skip
      in
      receive ~selector ()
