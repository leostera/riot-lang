module type Intf = sig
  type t

  val register: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Adapter.error) Result.t

  val reregister: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Adapter.error) Result.t

  val deregister: t -> Adapter.Selector.t -> (unit, Adapter.error) Result.t
end

type t

val make: (module Intf with type t = 'state) -> 'state -> t

val register: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Adapter.error) Result.t

val reregister: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Adapter.error) Result.t

val deregister: t -> Adapter.Selector.t -> (unit, Adapter.error) Result.t
