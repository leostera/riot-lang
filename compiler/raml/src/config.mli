type t = {
  on_event: (Event.t -> unit) option;
  host: Target.t;
  target: Target.t;
}
val default: t

val make: ?on_event:(Event.t -> unit) -> ?host:Target.t -> ?target:Target.t -> unit -> t

val with_on_event: t -> on_event:(Event.t -> unit) -> t

val without_on_event: t -> t

val with_host: t -> host:Target.t -> t

val with_target: t -> target:Target.t -> t

val with_targeting: t -> host:Target.t -> target:Target.t -> t

val emit_event: t -> (unit -> Event.kind) -> unit
