(** Worker actor for linting individual files *)
open Std

(** Worker configuration *)

(**
   Start a new worker actor.

   The worker will:
   1. Send WorkerReady message to coordinator
   2. Wait for RunTask or Stop message
   3. Lint the file
   4. Send results back to coordinator
   5. Repeat

   If linting fails, the worker sends a failure message and continues.
*)
type config = {
  mode: Runner.mode;
  scope: Fix_config.scope option;
  coordinator: Pid.t;
}

val start: config -> Pid.t
