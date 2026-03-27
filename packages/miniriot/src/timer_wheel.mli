(** Hierarchical timing wheel for efficient timer management *)

type t
(** A hierarchical timing wheel *)

val create : config:Config.t -> t
(** Create a new timing wheel with the given configuration *)

val add_timer :
  t ->
  now:int64 ->
  duration_nanos:int64 ->
  mode:Timer.mode ->
  action:Timer.action ->
  Timer.id
(** Add a timer to the wheel. Returns the timer ID for cancellation. *)

val reschedule_timer : t -> now:int64 -> Timer.t -> unit
(** Reinsert an existing interval timer without changing its ID. *)

val cancel_timer : t -> Timer.id -> unit
(** Cancel a timer by its ID *)

val tick : t -> now:int64 -> Timer.t list
(** Advance the wheel to the given time and return expired timers *)

val next_expiration : t -> now:int64 -> int64 option
(** Get the time (in nanos) of the next timer expiration, if any *)

val size : t -> int
(** Get the total number of active timers in the wheel *)
