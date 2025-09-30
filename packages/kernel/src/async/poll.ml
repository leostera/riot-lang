open Common

type t = { selector : Adapter.Selector.t }

let make () =
  let* selector = Adapter.Selector.make () in
  Ok { selector }

let poll ?max_events ?timeout t =
  Adapter.Selector.select ?timeout ?max_events t.selector

let register (t : t) token interests source =
  Source.register source t.selector token interests

let reregister (t : t) token interests source =
  Source.reregister source t.selector token interests

let deregister (t : t) source = Source.deregister source t.selector
