(** Tracing utilities for debugging *)

val enable : unit -> unit
(** Enable trace logging *)

val disable : unit -> unit
(** Disable trace logging *)

val trace : ('a, unit, string, unit) format4 -> 'a
(** Print a trace message if tracing is enabled *)
