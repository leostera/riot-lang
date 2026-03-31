open Common

module type Intf = sig
  type t
  val deregister : t -> Adapter.Selector.t -> (unit, IO.error) result

  val register : t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, IO.error) result

  val reregister : t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, IO.error) result
end

type t =
  S : ((module Intf with type t = 'state) * 'state) -> t

let make = fun src state -> S (src, state)

let register = fun (S ((module Src), state)) -> Src.register state

let reregister = fun (S ((module Src), state)) -> Src.reregister state

let deregister = fun (S ((module Src), state)) -> Src.deregister state
