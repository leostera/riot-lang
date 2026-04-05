(** # Process - Process primitives

    Process management helpers layered on top of the actors runtime.

    ## Example

    ```ocaml
    let child =
      Process.spawn (fun () ->
        Log.info "child started";
        Ok ())
    in
    ignore child
    ```
*)

(** Re-export of the core process API from [Actors.Process]. *)
include module type of Actors.Process

open Global

(** Returns the current process identifier. *)
val self: unit -> Pid.t

(** Spawns a new unlinked process that runs the given function. *)
val spawn: (unit -> (unit, exit_reason) result) -> Pid.t

(** Spawns a new process linked to the current process. *)
val spawn_link: (unit -> (unit, exit_reason) result) -> Pid.t
