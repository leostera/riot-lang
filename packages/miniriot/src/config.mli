(** Runtime configuration for Miniriot *)

type timer_resolution =
  | Second  (** Timer resolution in seconds - lowest overhead *)
  | Millisecond  (** Timer resolution in milliseconds - balanced *)
  | Microsecond  (** Timer resolution in microseconds - high precision *)
  | Nanosecond  (** Timer resolution in nanoseconds - highest precision *)

type t = { timer_resolution : timer_resolution }
(** Runtime configuration *)

val default : t
(** Default configuration with millisecond timer resolution *)

val make : ?timer_resolution:timer_resolution -> unit -> t
(** Create a configuration with custom settings *)

val resolution_to_nanos : timer_resolution -> int64
(** Convert resolution to nanoseconds per tick *)
