(** Date and time utilities *)

type t

val now : unit -> t
(** Get current time as Unix timestamp *)

val to_float : t -> float

val localtime : float -> Unix.tm
(** Convert timestamp to local time *)

val gmtime : float -> Unix.tm
(** Convert timestamp to GMT/UTC time *)
