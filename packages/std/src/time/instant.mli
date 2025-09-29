(** A measurement of a monotonically nondecreasing clock. Opaque and useful only
    with Duration.

    Instants are always guaranteed, barring platform bugs, to be no less than
    any previously measured instant when created, and are often useful for tasks
    such as measuring benchmarks or timing how long an operation takes.

    Note, however, that instants are not guaranteed to be steady. In other
    words, each tick of the underlying clock might not be the same length (e.g.
    some seconds may be longer than others). An instant may jump forwards or
    experience time dilation (slow down or speed up), but it will never go
    backwards. As part of this non-guarantee it is also not specified whether
    system suspends count as elapsed time or not. The behavior varies across
    platforms and Rust versions.

    Instants are opaque types that can only be compared to one another. There is
    no method to get "the number of seconds" from an instant. Instead, it only
    allows measuring the duration between two instants (or comparing two
    instants).

    The size of an Instant struct may vary depending on the target operating
    system. *)

type t

(** {1 Creation} *)

val now : unit -> t
(** Returns the current instant *)

(** {1 Duration Operations} *)

val duration_since : earlier:t -> t -> Duration.t
(** Returns the amount of time elapsed between two instants *)

val saturating_duration_since : earlier:t -> t -> Duration.t
(** Returns the amount of time elapsed from another instant to this one, or zero
    duration if that instant is later than this one *)

val elapsed : t -> Duration.t
(** Time elapsed since this instant *)

(** {1 Arithmetic Operations} *)

val add : t -> Duration.t -> t
(** Add a duration to an instant *)

val sub : t -> Duration.t -> t
(** Subtract a duration from an instant *)

(** {1 Checked Operations} *)

val checked_add : t -> Duration.t -> t option
(** Returns [Some result] if the result can be represented, [None] otherwise *)

val checked_sub : t -> Duration.t -> t option
(** Returns [Some result] if the result can be represented, [None] otherwise *)

(** {1 Comparison} *)

val compare : t -> t -> int
(** Compare two instants *)

val equal : t -> t -> bool
(** Test equality of two instants *)

val min : t -> t -> t
val max : t -> t -> t
