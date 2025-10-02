open Global
open Miniriot
open Types

type 'task state = {
  coordinator : Pid.t;
  owner : Pid.t;
  worker_fn : owner:Pid.t -> task:'task -> unit;
  task_ref : 'task Ref.t;
}

(** Worker loop - receives tasks from coordinator and executes them *)
let rec loop state =
  let selector msg =
    match msg with ToWorker (Task task) -> `select task | _ -> `skip
  in

  let task = receive ~selector () in
  match Task.value task state.task_ref with
  | Some task ->
      (* Execute the user's worker function *)
      state.worker_fn ~owner:state.owner ~task;

      (* Notify coordinator that we're done *)
      let handle = { pid = self (); task_ref = state.task_ref } in
      send state.coordinator (ToCoordinator (WorkerReady handle));

      (* Continue looping for more tasks *)
      loop state
  | None -> panic "Worker received task with mismatched type"

let init ~coordinator ~owner ~worker_fn ~task_ref () =
  let state = { coordinator; owner; worker_fn; task_ref } in
  loop state

let start ~coordinator ~owner ~worker_fn ~task_ref =
  spawn (init ~coordinator ~owner ~worker_fn ~task_ref)
