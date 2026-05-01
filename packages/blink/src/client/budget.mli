open Std

type policy = {
  capacity: int;
  window: Time.Duration.t;
}
type t

val policy: capacity:int -> window:Time.Duration.t -> policy

val create: capacity:int -> window:Time.Duration.t -> Time.Instant.t -> t

val create_with_policy: policy -> Time.Instant.t -> t

val capacity: t -> int

val remaining: t -> int

val allow: now:Time.Instant.t -> t -> bool

val reset_if_needed: now:Time.Instant.t -> t -> unit
