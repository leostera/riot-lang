(** Common types re-exported from Stdlib for use in nostdlib packages *)

include module type of Global0

val print : string -> unit
(** Print to stdout *)

val println : string ->  unit
(** Print to stdout with newline *)

val eprint : string -> unit
(** Print to stderr *)

val eprintln : string -> unit
(** Print to stderr with newline *)
