type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }
  | InvalidMaxEvents of { max_events: int }
  | System of System_error.t
val error_to_string: error -> string

module Token: sig
  type t

  (** Use `make value` to create a token that carries kernel-owned registration context. *)
  val make: 'value -> t

  (** Recover the value stored in a token when the caller already owns the registration site and
      therefore knows the token payload type. *)
  val unsafe_value: t -> 'value

  (** Use `id token` when you need a stable process-local identity for hashing or comparisons. *)
  val id: t -> int

  val hash: t -> int

  val equal: t -> t -> bool
end

module Interest: sig
  type t
  val readable: t

  val writable: t

  val priority: t

  (** Use `add left right` to combine readiness interests into one non-empty mask. *)
  val add: t -> t -> t

  (** Use `remove left right` to subtract interest bits.

      The result is `None` when every requested interest bit is removed. *)
  val remove: t -> t -> t option

  val is_readable: t -> bool

  val is_writable: t -> bool

  val is_priority: t -> bool
end

module Event: sig
  type t

  (** Use `token event` to recover the token that was registered for the ready source. *)
  val token: t -> Token.t

  val is_error: t -> bool

  val is_priority: t -> bool

  val is_read_closed: t -> bool

  val is_readable: t -> bool

  val is_writable: t -> bool

  val is_write_closed: t -> bool
end

module Adapter: sig
  (** Backend-facing selector plumbing used by kernel-owned source adapters.
      Application code should prefer `Poll` plus `to_source` values from public modules. *)
  module Selector: sig
    type t

    (** Use `make ()` to allocate the backend selector state immediately. *)
    val make: unit -> (t, error) Result.t

    (** Use `close selector` to release backend selector state immediately. *)
    val close: t -> (unit, error) Result.t

    (** Use `select selector` for one readiness polling pass. *)
    val select: ?timeout:int64 -> ?max_events:int -> t -> (Event.t list, error) Result.t

    (** Use `register selector ~fd ~token ~interest` to start tracking one file-descriptor source. *)
    val register: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, error) Result.t

    (** Use `reregister` to update the token or interests for an already-registered file-descriptor
        source. *)
    val reregister: t -> fd:int -> token:Token.t -> interest:Interest.t -> (unit, error) Result.t

    (** Use `deregister selector ~fd` to stop tracking one file-descriptor source. *)
    val deregister: t -> fd:int -> (unit, error) Result.t

    (** Use `register_process` to start tracking process-exit readiness. *)
    val register_process: t -> pid:int -> token:Token.t -> (unit, error) Result.t

    (** Use `reregister_process` to update the token for an already-registered process source. *)
    val reregister_process: t -> pid:int -> token:Token.t -> (unit, error) Result.t

    (** Use `deregister_process selector ~pid` to stop tracking one process source. *)
    val deregister_process: t -> pid:int -> (unit, error) Result.t

    (** Use `register_timer` to start tracking one timer source with already-split timeout parts. *)
    val register_timer:
      t ->
      timer_id:int ->
      token:Token.t ->
      timeout_parts:(int * int) ->
      repeat:bool ->
      (unit, error) Result.t

    (** Use `reregister_timer` to update an already-registered timer source. *)
    val reregister_timer:
      t ->
      timer_id:int ->
      token:Token.t ->
      timeout_parts:(int * int) ->
      repeat:bool ->
      (unit, error) Result.t

    (** Use `deregister_timer selector ~timer_id` to stop tracking one timer source. *)
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

  (** Backend-facing source constructor. Public consumers should prefer source
      values produced by `Fs.File.to_source`, `Net.*.to_source`, `Process.to_source`,
      and `Time.Timer.to_source`. *)
  val make: (module Intf with type t = 'state) -> 'state -> t
end

module Poll: sig
  type t

  (** Use `make ()` to allocate a poller backed by the current platform selector. *)
  val make: unit -> (t, error) Result.t

  (** Use `close poll` to release selector state immediately. *)
  val close: t -> (unit, error) Result.t

  (** Use `poll poll` for one readiness polling pass across all registered sources. *)
  val poll: ?max_events:int -> ?timeout:int64 -> t -> (Event.t list, error) Result.t

  (** Use `register poll token interest source` to start tracking one public source. *)
  val register: t -> Token.t -> Interest.t -> Source.t -> (unit, error) Result.t

  (** Use `reregister` to update the token or interests for an already-registered source. *)
  val reregister: t -> Token.t -> Interest.t -> Source.t -> (unit, error) Result.t

  (** Use `deregister poll source` to stop tracking one public source. *)
  val deregister: t -> Source.t -> (unit, error) Result.t
end
