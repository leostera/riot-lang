(** # Actor - Actor runtime primitives

    Actor management helpers layered on top of the std-owned runtime.

    ## Example

    ```ocaml
    let child =
      Actor.spawn (fun () ->
        Log.info "child started";
        Ok ())
    in
    ignore child
    ```
*)

open Kernel

(** Re-export of the core actor API from [Runtime.Actor]. *)
include module type of Runtime.Actor

open Global

(** Returns the current actor identifier. *)
val self: unit -> Pid.t

(** Spawns a new unlinked actor that runs the given function. *)
val spawn: (unit -> (unit, exit_reason) result) -> Pid.t

(** Spawns a new actor linked to the current actor. *)
val spawn_link: (unit -> (unit, exit_reason) result) -> Pid.t
