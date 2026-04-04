open Std

(** Mutable prototype type representation used inside one inference query. *)
type label =
  | Nolabel
  | Labelled of string
  | Optional of string

type var = {
  id: int;
  mutable link: t option;
}

and t =
  | Int
  | Float
  | Bool
  | String
  | Unit
  | Option of t
  | Result of t * t
  | Array of t
  | Tuple of t list
  | Arrow of { label: label; lhs: t; rhs: t }
  | Var of var
  | Hole of int
val prune: t -> t

val union: int list -> int list -> int list

val diff: int list -> int list -> int list

val free_vars: t -> int list

val occurs: int -> t -> bool
