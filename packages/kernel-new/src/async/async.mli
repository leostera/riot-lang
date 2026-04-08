module Token: sig
  type t
  val hash: t -> int

  val equal: ?eq:('a -> 'a -> bool) -> t -> t -> bool

  val make: 'value -> t
end

module Interest: sig
  type t
  val readable: t

  val writable: t

  val add: t -> t -> t

  val remove: t -> t -> t option

  val is_readable: t -> bool

  val is_writable: t -> bool
end

module Event: sig
  type t
  val token: t -> Token.t

  val is_error: t -> bool

  val is_priority: t -> bool

  val is_read_closed: t -> bool

  val is_readable: t -> bool

  val is_writable: t -> bool

  val is_write_closed: t -> bool
end

module Adapter: sig
  module Selector: sig
    type t
    val make: unit -> (t, Error.t) Result.t

    val select: ?timeout:int64 -> ?max_events:int -> t -> (Event.t list, Error.t) Result.t

    val register: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, Error.t) Result.t

    val reregister: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, Error.t) Result.t

    val deregister: t -> fd:int -> (unit, Error.t) Result.t
  end
end

module Source: sig
  type t
  module type Intf = sig
    type t
    val register: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Error.t) Result.t

    val reregister: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, Error.t) Result.t

    val deregister: t -> Adapter.Selector.t -> (unit, Error.t) Result.t
  end

  val make: (module Intf with type t = 'state) -> 'state -> t
end

module Poll: sig
  type t
  val make: unit -> (t, Error.t) Result.t

  val poll: ?max_events:int -> ?timeout:int64 -> t -> (Event.t list, Error.t) Result.t

  val register: t -> Token.t -> Interest.t -> Source.t -> (unit, Error.t) Result.t

  val reregister: t -> Token.t -> Interest.t -> Source.t -> (unit, Error.t) Result.t

  val deregister: t -> Source.t -> (unit, Error.t) Result.t
end
