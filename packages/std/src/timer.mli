type id = Miniriot.Timer.id
(** Opaque timer identifier *)

val send_after : Pid.t -> Message.t -> after:Time.Duration.t -> id
(** Send a message to a process after a delay. Returns a timer ID
    that can be used to cancel the timer. *)

val send_interval : Pid.t -> Message.t -> interval:Time.Duration.t -> id
(** Send a message to a process repeatedly at a given interval.
    Returns a timer ID that can be used to cancel the timer. *)

val cancel : id -> unit
(** Cancel a timer by its ID. If the timer has already fired or doesn't exist,
    this is a no-op. *)

val equal : id -> id -> bool
(** Compare two timer IDs for equality *)
