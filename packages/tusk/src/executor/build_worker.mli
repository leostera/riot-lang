(** Build worker - executes build tasks in sandboxes *)

open Miniriot

val main : Worker_pool_types.ctx -> unit -> (unit, Process.exit_reason) result
(** Main worker function *)
