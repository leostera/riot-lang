(** Atomic references. *)
type !'value t
val make: 'value -> 'value t

val make_contended: 'value -> 'value t

val get: 'value t -> 'value

val set: 'value t -> 'value -> unit

val exchange: 'value t -> 'value -> 'value

val compare_and_set: 'value t -> 'value -> 'value -> bool

val fetch_and_add: int t -> int -> int

val incr: int t -> unit

val decr: int t -> unit

module Loc: sig
  type 'value t = 'value atomic_loc

  external get: 'value t -> 'value = "%atomic_load_loc"

  val set: 'value t -> 'value -> unit

  external exchange: 'value t -> 'value -> 'value = "%atomic_exchange_loc"

  external compare_and_set: 'value t -> 'value -> 'value -> bool = "%atomic_cas_loc"

  external fetch_and_add: int t -> int -> int = "%atomic_fetch_add_loc"

  val incr: int t -> unit

  val decr: int t -> unit
end
