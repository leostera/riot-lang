type t
type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }
val error_to_string: error -> string

(** [after_ns timeout] constructs a one-shot timer source.

    A one-shot timer becomes readable at most once per registration. Callers may register the same
    source again after it fires to arm a fresh one-shot wait. *)
val after_ns: int64 -> (t, error) Result.t

(** [every_ns timeout] constructs a repeating timer source that stays readable on each interval
    until it is deregistered. *)
val every_ns: int64 -> (t, error) Result.t

val timeout_ns: t -> int64

val repeats: t -> bool

(** [to_source timer] exposes the timer through [Async.Poll]. Registering or reregistering the
    source arms the current timer configuration. *)
val to_source: t -> Async.Source.t
