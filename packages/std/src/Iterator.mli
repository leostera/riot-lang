module type Intf = sig
  type state
  type item

  val next : state -> item option * state
  val size : state -> int
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)

type 'item t

val make : ('item, 'state) iter -> 'state -> 'item t
val next : 'item t -> 'item option * 'item t
val size : 'item t -> int
val to_list : 'item t -> 'item list
