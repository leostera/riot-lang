open Std

(** {1 Value - Datalog Constants}
    
    Represents concrete values that can appear in Datalog facts.
    These are the "atoms" of our system - the actual data.
*)

type t =
  | Int of int        (** Integer constant: 42, -10, 0 *)
  | String of string  (** String constant: "hello", "alice" *)
  | Uri of string     (** URI constant: "uri:module:foo" (for Poneglyph) *)

val compare : t -> t -> int
(** Total ordering for values. Required for sorted relations. *)

val equal : t -> t -> bool
(** Equality check *)

val to_string : t -> string
(** Convert value to human-readable string *)

val hash : t -> int
(** Hash function for use in HashMaps *)
