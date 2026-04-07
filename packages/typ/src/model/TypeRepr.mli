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

and desc =
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Option of t
  | Result of t * t
  | Array of t
  | List of t
  | Seq of t
  | Named of {
      type_constructor_id: TypeConstructorId.t option;
      name: IdentPath.t;
      arguments: t list
    }
  | Tuple of t list
  | Arrow of { label: label; lhs: t; rhs: t }
  | Var of var
  | Hole of int

and t = {
  mutable desc: desc;
  mutable level: int;
  mutable mark: int;
  mutable mark_order: int;
}
val int: t

val float: t

val bool: t

val string: t

val char: t

val unit_: t

val option: t -> t

val result: t -> t -> t

val array: t -> t

val list: t -> t

val seq: t -> t

val named:
  type_constructor_id:TypeConstructorId.t option -> name:IdentPath.t -> arguments:t list -> t

val tuple: t list -> t

val arrow: label:label -> lhs:t -> rhs:t -> t

val hole: int -> t

val of_desc: ?level:int -> desc -> t

val prune: t -> t

val view: t -> desc

val level: t -> int

val set_level: t -> int -> unit

val generic_level: int

val is_generic_level: int -> bool

val make_var: ?level:int -> int -> t

val is_generic_var: t -> bool

val set_generic_var: t -> unit

val seal_levels: t -> unit

val generalize_ids: int list -> t -> unit

val generic_var_ids: t -> int list

val union: int list -> int list -> int list

val diff: int list -> int list -> int list

val free_vars: t -> int list

val mark_reachable_vars: generation:int -> next_order:(unit -> int) -> t -> unit

val covariant_vars: t -> int list

val occurs: int -> t -> bool

val occurs_or_lower:
  generation:int -> needle:int -> level:int -> on_lower:(t -> unit) -> t -> bool
