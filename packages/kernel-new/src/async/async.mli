type error =
  | Invalid_timeout_ns of { timeout_ns: int64 }
  | Invalid_max_events of { max_events: int }
  | System of System_error.t
val error_to_string: error -> string

module Token: sig
  type t
  val hash: t -> int

  val equal: ?eq:('a -> 'a -> bool) -> t -> t -> bool

  val make: 'value -> t

  val unsafe_to_value: t -> 'value
end

module Interest: sig
  type t
  val readable: t

  val writable: t

  val priority: t

  val add: t -> t -> t

  val remove: t -> t -> t option

  val is_readable: t -> bool

  val is_writable: t -> bool

  val is_priority: t -> bool
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
    val make: unit -> (t, error) Result.t

    val close: t -> (unit, error) Result.t

    val select: ?timeout:int64 -> ?max_events:int -> t -> (Event.t list, error) Result.t

    val register: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, error) Result.t

    val reregister: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, error) Result.t

    val deregister: t -> fd:int -> (unit, error) Result.t

    val register_process: t -> pid:int -> token:Token.t -> (unit, error) Result.t

    val reregister_process: t -> pid:int -> token:Token.t -> (unit, error) Result.t

    val deregister_process: t -> pid:int -> (unit, error) Result.t

    val register_timer:
      t -> timer_id:int -> token:Token.t -> timeout_parts:(int * int) -> repeat:bool -> (unit, error) Result.t

    val reregister_timer:
      t -> timer_id:int -> token:Token.t -> timeout_parts:(int * int) -> repeat:bool -> (unit, error) Result.t

    val deregister_timer: t -> timer_id:int -> (unit, error) Result.t
  end
end

module Source: sig
  type t
  module type Intf = sig
    type t
    val register: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, error) Result.t

    val reregister: t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, error) Result.t

    val deregister: t -> Adapter.Selector.t -> (unit, error) Result.t
  end

  val make: (module Intf with type t = 'state) -> 'state -> t
end

module Poll: sig
  type t
  val make: unit -> (t, error) Result.t

  val close: t -> (unit, error) Result.t

  val poll: ?max_events:int -> ?timeout:int64 -> t -> (Event.t list, error) Result.t

  val register: t -> Token.t -> Interest.t -> Source.t -> (unit, error) Result.t

  val reregister: t -> Token.t -> Interest.t -> Source.t -> (unit, error) Result.t

  val deregister: t -> Source.t -> (unit, error) Result.t
end
