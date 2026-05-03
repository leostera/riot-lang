(**
   Mutable iteration protocol.

   Mutable iterator protocol for efficient sequence processing. Calling
   [next] mutates the internal state.
*)
module type Intf = sig
  type state
  type item

  val next: state -> item option

  val size: state -> int

  val clone: state -> state
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)
type 'item t

val empty: unit -> 'item t

val singleton: 'item -> 'item t

val make: ('item, 'state) iter -> 'state -> 'item t

val next: 'item t -> 'item option

val size: 'item t -> int

val clone: 'item t -> 'item t

val collect: 'item t -> 'item list -> 'item list

val to_list: 'item t -> 'item list

val map: 'a t -> fn:('a -> 'b) -> 'b t

val filter: 'a t -> fn:('a -> bool) -> 'a t

val filter_map: 'a t -> fn:('a -> 'b option) -> 'b t

val flat_map: 'a t -> fn:('a -> 'b t) -> 'b t

val fold: 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc

val reduce: 'a t -> fn:('a -> 'a -> 'a) -> 'a option

val count: 'a t -> int

val find: 'a t -> fn:('a -> bool) -> 'a option

val any: 'a t -> fn:('a -> bool) -> bool

val all: 'a t -> fn:('a -> bool) -> bool

val take: 'a t -> int -> 'a t

val drop: 'a t -> int -> 'a t

val enumerate: 'a t -> (int * 'a) t

val zip: 'a t -> 'b t -> ('a * 'b) t

val chain: 'a t -> 'a t -> 'a t

val for_each: 'a t -> fn:('a -> unit) -> unit
