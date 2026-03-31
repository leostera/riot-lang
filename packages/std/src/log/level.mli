(** Log severity levels *)
(** Log levels from least to most severe *)
(** Convert level to integer for comparison *)
type t =
  | Trace
  | Debug
  | Info
  | Warn
  | Error
val to_int: t -> int

(** Convert level to string representation *)
val to_string: t -> string

(** Compare two levels *)
val compare: t -> t -> int
