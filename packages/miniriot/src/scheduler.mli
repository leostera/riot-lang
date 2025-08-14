(** Process scheduler for Miniriot *)

type t
(** Opaque scheduler type *)

val get_scheduler : unit -> t
(** Get the current scheduler (must be called within a running scheduler) *)

val spawn : t -> (unit -> Process.exit_reason) -> Pid.t
(** Spawn a new process with the given function *)

val self : unit -> Pid.t
(** Get the PID of the currently running process *)

val send : Pid.t -> Message.t -> unit
(** Send a message to a process *)

val shutdown : t -> status:int -> unit
(** Shutdown the scheduler with given exit status *)

val run : main:(unit -> Process.exit_reason) -> int
(** Run the scheduler with the given main function. Returns exit status.
    Can only be called once per process. *)