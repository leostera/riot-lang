open Std

type t = {
  on_event: (Event.t -> unit) option;
  host: Target.t;
  target: Target.t;
  content_store: Contentstore.t option;
}
val default: t

val validate: t -> (unit, string) Std.Result.t

val make:
  ?on_event:(Event.t -> unit) ->
  ?host:Target.t ->
  ?target:Target.t ->
  ?content_store:Contentstore.t ->
  unit ->
  t

val with_on_event: t -> on_event:(Event.t -> unit) -> t

val without_on_event: t -> t

val with_host: t -> host:Target.t -> t

val with_target: t -> target:Target.t -> t

val with_targeting: t -> host:Target.t -> target:Target.t -> t

val with_content_store: t -> content_store:Contentstore.t -> t

val without_content_store: t -> t

val host: t -> Target.t

val target: t -> Target.t

val content_store: t -> Contentstore.t option

val select_backend: t -> Target.backend

val emit_event: t -> (unit -> Event.kind) -> unit
