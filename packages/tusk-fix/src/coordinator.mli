(** Coordinator actor for managing lint workers *)

open Std

type input =
  | Files of Path.t list
  | Roots of Path.t list
(** Either a pre-resolved file list or root paths to stream from. *)

type config = {
  input : input;
  concurrency : int;
  limit : int option;
  mode : Runner.mode;
  scope : Fix_config.scope option;
  owner : Pid.t;
}
(** Coordinator configuration *)

val start : config -> Pid.t
(** Start a new coordinator actor.
    
    The coordinator will:
    1. Create a queue of files to lint, optionally fed by a streaming scanner
    2. Spawn N worker actors
    3. Distribute work to idle workers
    4. Stream file results back to the owner actor
    5. Track completion
    6. Stop workers when the queue drains or early termination is requested
    7. Send AllComplete message to owner when done
*)
