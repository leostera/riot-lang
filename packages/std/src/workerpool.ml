(** Generic worker pool for concurrent task processing 

open Miniriot

module type Intf = sig
end

module type Base = sig
  type task
  type result
end

module Make (B: Base): Intf = struct

type ctx = B.ctx
type task = B.task
type result = B.result

  type Message.t += 
    | WorkerReady of Pid.t
    | NextTask of task
    | TaskCompleted of result

  module Worker = struct
    type state = {
      pool: Pid.t; 
      ctx: ctx;
      fn: (ctx, task) -> result
    }

    let rec loop state =
      send state.pool (WorkerReady (self ()));
      let selector msg = 
        match msg with
        | NextTask task -> `select task
        | _ -> `skip
      in
      let task = receive ~selector () in
      let result = state.fn task in
      send state.pool (TaskCompleted result);
      loop state

    let start pool ctx fn = spawn (loop {pool; ctx; fn})
  end

  (* Simple implementation: just process tasks sequentially for now *)
  type ('ctx, 'task, 'result) t = unit

  let start ~workers ~ctx ~worker_fn ~on_worker_ready ~on_task_completed =
    (*

    1. start `workers` processes
    2. enter a receive loop where we wait for

    * WorkerReady(pid) message, and when it arrives we call the task_provider to
    get a new task, and if we receive a task we send it to the worker pid in NextTask(task)

    * TaskComplete(result) -- we 

    *)
    Obj.magic false
end

*)
