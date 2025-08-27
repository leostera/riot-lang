(** Date and time utilities *)

val now : unit -> float
(** Get current time as Unix timestamp *)

val localtime : float -> Unix.tm
(** Convert timestamp to local time *)

val gmtime : float -> Unix.tm
(** Convert timestamp to GMT/UTC time *)
