(** Process identifiers *)
(** Opaque process identifier *)
(** The PID of the main process *)
type t
val main: t
(** Generate the next unique PID *)
val next: unit -> t
(** Test PID equality *)
val equal: t -> t -> bool
(** Compare PIDs for ordering *)
val compare: t -> t -> int
(** Convert PID to its underlying integer identifier. *)
val to_int: t -> int
(** Convert PID to string representation *)
val to_string: t -> string
