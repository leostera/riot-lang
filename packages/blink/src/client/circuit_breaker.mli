open Std

type state =
  | Closed
  | Open
  | HalfOpen
type policy = {
  failure_threshold: int;
  reset_after: Time.Duration.t;
}
type t
val policy: ?failure_threshold:int -> ?reset_after:Time.Duration.t -> unit -> policy

val default_policy: policy

val create: ?policy:policy -> unit -> t

val state: t -> state

val state_to_string: state -> string

val allow_request: now:Time.Instant.t -> t -> bool

val record_success: t -> unit

val record_failure: now:Time.Instant.t -> t -> unit

val consecutive_failures: t -> int
