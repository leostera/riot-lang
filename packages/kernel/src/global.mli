(** Common types re-exported from Stdlib for use in nostdlib packages *)
include module type of Global0
(** Print to stdout *)
val print: string -> unit
(** Print to stdout with newline *)
val println: string -> unit
(** Print to stderr *)
val eprint: string -> unit
(** Print to stderr with newline *)
val eprintln: string -> unit
