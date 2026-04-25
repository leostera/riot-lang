(** Coordinator actor for managing lint workers *)
open Std

(** Either a pre-resolved file list or root paths to stream from. *)
(** Coordinator configuration *)
type input =
  | Files of Path.t list
  | Roots of Path.t list

(**
   Start a new coordinator actor.

   The coordinator will:
   1. Create a queue of files to lint, optionally fed by a streaming scanner
   2. Spawn N worker actors
   3. Distribute work to idle workers
   4. Stream file results back to the owner actor
   5. Track completion
   6. Stop workers when the queue drains or early termination is requested
   7. Send AllComplete message to owner when done
*)
type config = {
  input: input;
  concurrency: int;
  limit: int option;
  mode: Runner.mode;
  scope: Fix_config.scope option;
  owner: Pid.t;
}

val start: config -> Pid.t
