module type Intf = sig
  type t

  val register:
    t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Error.t) Result.t

  val reregister:
    t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Error.t) Result.t

  val deregister:
    t -> Adapter.Selector.t -> (unit, Error.t) Result.t
end

type t =
  | S: (module Intf with type t = 'state) * 'state -> t

let make = fun implementation state -> S (implementation, state)

let register = fun (S ((module Source), state)) -> Source.register state

let reregister = fun (S ((module Source), state)) -> Source.reregister state

let deregister = fun (S ((module Source), state)) -> Source.deregister state
