module type Intf = sig
  type state
  type item

  val next : state -> item option
  val size : state -> int
  val clone : state -> state
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)

type 'item t = Iter : (('item, 'state) iter * 'state) -> 'item t

val make : ('item, 'state) iter -> 'state -> 'item t
val next : 'item t -> 'item option
val size : 'item t -> int
val clone : 'item t -> 'item t
val collect : 'item t -> 'item list -> 'item list
val to_list : 'item t -> 'item list
