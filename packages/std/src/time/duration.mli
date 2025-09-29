(** A Duration type to represent a span of time, typically used for system
    timeouts.

    Each Duration is composed of a whole number of seconds and a fractional part
    represented in nanoseconds. If the underlying system does not support
    nanosecond-level precision, APIs binding a system timeout will typically
    round up the number of nanoseconds.

    Durations implement many common traits, including Add, Sub, and other ops
    traits. It implements Default by returning a zero-length Duration. *)

type t

(** {1 Constants} *)

val zero : t
(** A duration of zero time *)

val max : t
(** The maximum duration representable *)

(** {1 Creation} *)

val make : secs:int -> nanos:int -> t
(** Create a duration from seconds and nanoseconds *)

val from_days : int -> t
val from_hours : int -> t
val from_mins : int -> t
val from_secs : int -> t
val from_millis : int -> t
val from_micros : int -> t
val from_nanos : int -> t
val from_weeks : int -> t
val from_secs_float : float -> t

(** {1 Conversion} *)

val to_secs : t -> int
val to_secs_float : t -> float
val to_millis : t -> int
val to_micros : t -> int
val to_nanos : t -> int

(** {1 Subsecond Components} *)

val subsec_millis : t -> int
(** Returns the fractional part of this Duration in milliseconds (0-999) *)

val subsec_micros : t -> int
(** Returns the fractional part of this Duration in microseconds (0-999,999) *)

val subsec_nanos : t -> int
(** Returns the fractional part of this Duration in nanoseconds (0-999,999,999)
*)

(** {1 Predicates} *)

val is_zero : t -> bool

(** {1 Arithmetic Operations} *)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> int -> t
val div : t -> int -> t

(** {1 Checked Operations} *)

val checked_add : t -> t -> t option
(** Returns [Some result] if addition is representable, [None] otherwise *)

val checked_sub : t -> t -> t option
(** Returns [Some result] if subtraction would be positive, [None] otherwise *)

val checked_mul : t -> int -> t option
(** Returns [Some result] if multiplication is representable, [None] otherwise
*)

val checked_div : t -> int -> t option
(** Returns [Some result] if divisor is non-zero, [None] otherwise *)

(** {1 Saturating Operations} *)

val saturating_add : t -> t -> t
(** Addition that saturates at [max] on overflow *)

val saturating_sub : t -> t -> t
(** Subtraction that saturates at [zero] on underflow *)

val saturating_mul : t -> int -> t
(** Multiplication that saturates at [max] on overflow *)

(** {1 Floating Point Operations} *)

val mul_f64 : t -> float -> t
val div_f64 : t -> float -> t

(** {1 Utility} *)

val abs_diff : t -> t -> t
(** Absolute difference between two durations *)

val min : t -> t -> t
val max : t -> t -> t

val compare : t -> t -> int
(** Compare two durations *)

val equal : t -> t -> bool
(** Test equality of two durations *)
