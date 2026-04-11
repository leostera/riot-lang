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

let make = fun ~config ~source ->
  { source; config; host = Config.host config; target = Config.target config }

let source = fun ctx -> ctx.source

let config = fun ctx -> ctx.config

let host = fun ctx -> ctx.host

let target = fun ctx -> ctx.target
