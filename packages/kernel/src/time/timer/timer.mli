type t

type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }

val error_to_string: error -> string

(**
   Use `after_ns timeout` to construct a one-shot timer source.

   A one-shot timer becomes readable at most once per registration. Callers may register the same
   source again after it fires to arm a fresh one-shot wait.
*)
val after_ns: int64 -> (t, error) Result.t

(**
   Use `every_ns timeout` to construct a repeating timer source that stays readable on each
   interval until it is deregistered.
*)
val every_ns: int64 -> (t, error) Result.t

(** Use `timeout_ns timer` to recover the configured interval in nanoseconds. *)
val timeout_ns: t -> int64

(** Use `repeats timer` to check whether the timer is repeating or one-shot. *)
val repeats: t -> bool

(**
   Use `to_source timer` to expose the timer through `Async.Poll`.

   Registering or reregistering the source arms the current timer configuration.
*)
val to_source: t -> Async.Source.t
