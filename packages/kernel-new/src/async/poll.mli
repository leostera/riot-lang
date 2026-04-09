type t = {
  selector: Adapter.Selector.t;
}
val make: unit -> (t, Adapter.error) Result.t

val close: t -> (unit, Adapter.error) Result.t

val poll: ?max_events:int -> ?timeout:int64 -> t -> (Event.t list, Adapter.error) Result.t

val register: t -> Token.t -> Interest.t -> Source.t -> (unit, Adapter.error) Result.t

val reregister: t -> Token.t -> Interest.t -> Source.t -> (unit, Adapter.error) Result.t

val deregister: t -> Source.t -> (unit, Adapter.error) Result.t
