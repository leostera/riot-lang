(** Hierarchical timing wheel for efficient timer management *)
(** A hierarchical timing wheel *)
(** Create a new timing wheel with the given configuration *)
type t
val create: config:Config.t -> t
(** Add a timer to the wheel. Returns the timer ID for cancellation. *)
val add_timer:
  t -> now:int64 -> duration_nanos:int64 -> mode:Timer.mode -> action:Timer.action -> Timer.id
(** Reinsert an existing interval timer without changing its ID. *)
val reschedule_timer: t -> now:int64 -> Timer.t -> unit
(** Cancel a timer by its ID *)
val cancel_timer: t -> Timer.id -> unit
(** Advance the wheel to the given time and return expired timers *)
val tick: t -> now:int64 -> Timer.t list
(** Get the time (in nanos) of the next timer expiration, if any *)
val next_expiration: t -> now:int64 -> int64 option
(** Get the total number of active timers in the wheel *)
val size: t -> int
