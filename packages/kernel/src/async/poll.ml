open Common

type t = { selector: Adapter.Selector.t }

let make = fun () ->
  let* selector = Adapter.Selector.make () in Result.Ok { selector }

let close = fun t -> Adapter.Selector.close t.selector

let poll = fun ?max_events ?timeout t -> Adapter.Selector.select ?timeout ?max_events t.selector

let register = fun t token interest source -> Source.register source t.selector token interest

let reregister = fun t token interest source -> Source.reregister source t.selector token interest

let deregister = fun t source -> Source.deregister source t.selector
