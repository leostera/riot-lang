open Std

type t = {
  on_event: (Event.t -> unit) option;
  host: Target.t;
  target: Target.t;
  typing_config: Typ.Config.t;
}
val default: t

val validate: t -> (unit, string) Std.Result.t

val make:
  ?on_event:(Event.t -> unit) ->
  ?host:Target.t ->
  ?target:Target.t ->
  ?typing_config:Typ.Config.t ->
  unit ->
  t

val with_on_event: t -> on_event:(Event.t -> unit) -> t

val without_on_event: t -> t

val with_host: t -> host:Target.t -> t

val with_target: t -> target:Target.t -> t

val with_targeting: t -> host:Target.t -> target:Target.t -> t

val with_typing_config: t -> typing_config:Typ.Config.t -> t

val host: t -> Target.t

val target: t -> Target.t

val typing_config: t -> Typ.Config.t

val select_backend: t -> Target.backend

val emit_event: t -> (unit -> Event.kind) -> unit
