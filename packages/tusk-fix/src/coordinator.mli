(** Coordinator actor for managing lint workers *)

open Std

type config = {
  files : Path.t list;
  concurrency : int;
  mode : Runner.mode;
  scope : Fix_config.scope option;
  owner : Pid.t;
}
(** Coordinator configuration *)

val start : config -> Pid.t
(** Start a new coordinator actor.
    
    The coordinator will:
    1. Create a queue of files to lint
    2. Spawn N worker actors
    3. Distribute work to idle workers
    4. Receive and print diagnostics as they arrive
    5. Track completion
    6. Send AllComplete message to owner when done
*)
