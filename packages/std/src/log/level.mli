(** Log severity levels *)

type t = Trace | Debug | Info | Warn | Error
(** Log levels from least to most severe *)

val to_int : t -> int
(** Convert level to integer for comparison *)

val to_string : t -> string
(** Convert level to string representation *)

val compare : t -> t -> int
(** Compare two levels *)
