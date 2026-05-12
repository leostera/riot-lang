(** This module carries the shared compiler context that backend pipelines need
    to make compilation decisions.

    The context keeps the selected source unit together with the compiler
    configuration and the resolved host and target triples derived from that
    configuration.

    The point is to give every backend one typed value to thread through its
    pipeline instead of re-plumbing host, target, and source metadata as
    separate arguments whenever another pass needs them. *)
type t = {
  source: Source_unit.t;
  config: Config.t;
  host: Target.t;
  target: Target.t;
}
val make: config:Config.t -> source:Source_unit.t -> t

val source: t -> Source_unit.t

val config: t -> Config.t

val host: t -> Target.t

val target: t -> Target.t
