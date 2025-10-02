open Global
open Miniriot
open Types

type 'task state = {
  coordinator : Pid.t;
  owner : Pid.t;
  worker_fn : owner:Pid.t -> task:'task -> unit;
  ref : 'task Ref.t;
}

(** Worker loop - receives tasks from coordinator and executes them *)
let rec loop (state : 'task state) : (unit, Process.exit_reason) result =
  let selector msg =
    match msg with Types.ToWorker msg -> `select msg | _ -> `skip
  in

  match receive ~selector () with
  | Types.Stop -> Ok ()
  | Types.Task (task, ref) -> (
      (* Verify type safety with Ref.type_equal *)
      match Ref.type_equal state.ref ref with
      | Some Type.Equal ->
          (* Execute the user's worker function *)
          state.worker_fn ~owner:state.owner ~task;

          (* Notify coordinator that we're done *)
          send state.coordinator
            (Types.FromWorker (Types.TaskCompleted (self ())));

          (* Continue looping for more tasks *)
          loop state
      | None -> panic "Worker received task with mismatched type")
