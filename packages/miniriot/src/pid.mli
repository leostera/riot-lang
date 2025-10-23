(** Process identifiers *)

type t
(** Opaque process identifier *)

val zero : t
(* The zero pid *)

val main : t
(** The PID of the main process *)

val next : unit -> t
(** Generate the next unique PID *)

val equal : t -> t -> bool
(** Test PID equality *)

val compare : t -> t -> int
(** Compare PIDs for ordering *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a PID *)

val to_string : t -> string
(** Convert PID to string representation *)
