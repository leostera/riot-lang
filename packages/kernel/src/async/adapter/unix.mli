type error =
  | InvalidTimeoutNs of {
      timeout_ns: int64;
    }
  | InvalidMaxEvents of { max_events: int }
  | System of System_error.t

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
    t ->
    timer_id:int ->
    token:Token.t ->
    timeout_parts:(int * int) ->
    repeat:bool ->
    (unit, error) Result.t

  val reregister_timer:
    t ->
    timer_id:int ->
    token:Token.t ->
    timeout_parts:(int * int) ->
    repeat:bool ->
    (unit, error) Result.t

  val deregister_timer: t -> timer_id:int -> (unit, error) Result.t
end
