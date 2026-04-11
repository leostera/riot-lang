open Std

(** Host configuration for running the `typ` checker stack. *)
type t = {
  (** Whether checker and inference layers should retain trace payloads. *)
  capture_traces: bool;
  (** Optional structured event sink. *)
  on_event: (Event.t -> unit) option;
}

val default: t

val with_capture_traces: t -> capture_traces: bool -> t

val with_on_event: t -> on_event: (Event.t -> unit) -> t

val without_on_event: t -> t

(** Emit one structured event when the config carries a sink. The thunk
    receives the monotonic timestamp that should be embedded in the event. *)
val emit_event: t -> (instant_us:int -> Event.t) -> unit
