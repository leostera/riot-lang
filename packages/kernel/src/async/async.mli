val syscall : (unit -> 'a IO.io_result) -> 'a IO.io_result

module Token : sig
  type t

  val hash : t -> int
  val equal : ?eq:('a -> 'a -> bool) -> t -> t -> bool
  val make : 'value -> t
  val pp : Format.formatter -> t -> unit
  val unsafe_to_value : t -> 'value
end

module Interest : sig
  type t

  val add : t -> t -> t
  val is_readable : t -> bool
  val is_writable : t -> bool
  val readable : t
  val remove : t -> t -> t option
  val writable : t
end

module Event : sig
  module type Intf = sig
    type t

    val is_error : t -> bool
    val is_priority : t -> bool
    val is_read_closed : t -> bool
    val is_readable : t -> bool
    val is_writable : t -> bool
    val is_write_closed : t -> bool
    val token : t -> Token.t
  end

  type t

  val is_error : t -> bool
  val is_priority : t -> bool
  val is_read_closed : t -> bool
  val is_readable : t -> bool
  val is_writable : t -> bool
  val is_write_closed : t -> bool
  val make : (module Intf with type t = 'state) -> 'state -> t
  val token : t -> Token.t
end

module Adapter : sig
  module Selector : sig
    type t

    val name : string
    val make : unit -> (t, IO.error) result

    val select :
      ?timeout:int64 ->
      ?max_events:int ->
      t ->
      (Event.t list, IO.error) result

    val register :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      unit IO.io_result

    val reregister :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      unit IO.io_result

    val deregister : t -> fd:Fd.t -> unit IO.io_result
  end

  module Event : sig
    type t
  end
end

module Source : sig
  module type Intf = sig
    type t

    val deregister : t -> Adapter.Selector.t -> unit IO.io_result

    val register :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      unit IO.io_result

    val reregister :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      unit IO.io_result
  end

  type t = S : ((module Intf with type t = 'state) * 'state) -> t

  val deregister : t -> Adapter.Selector.t -> unit IO.io_result
  val make : (module Intf with type t = 'a) -> 'a -> t

  val register :
    t ->
    Adapter.Selector.t ->
    Token.t ->
    Interest.t ->
    unit IO.io_result

  val reregister :
    t ->
    Adapter.Selector.t ->
    Token.t ->
    Interest.t ->
    unit IO.io_result
end

module Poll : sig
  type t

  val make : unit -> (t, IO.error) result

  val poll :
    ?max_events:int ->
    ?timeout:int64 ->
    t ->
    (Event.t list, IO.error) result

  val register :
    t -> Token.t -> Interest.t -> Source.t -> unit IO.io_result

  val reregister :
    t -> Token.t -> Interest.t -> Source.t -> unit IO.io_result

  val deregister : t -> Source.t -> unit IO.io_result
end
