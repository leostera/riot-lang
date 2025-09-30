(** Time operations for Kernel *)

val time : unit -> float
(** Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds *)

val gettimeofday : unit -> float
(** Same as {!time}, but with microsecond precision *)

val localtime : float -> Unix.tm
(** Convert a time in seconds to a date and time in the local time zone *)

val gmtime : float -> Unix.tm
(** Convert a time in seconds to a date and time in UTC *)

val mktime : Unix.tm -> float * Unix.tm
(** Convert a date and time to a time in seconds *)
