open Common

type t = {
  selector: Adapter.Selector.t;
}

let make = fun () ->
  let* selector = Adapter.Selector.make () in
  Ok {selector}

let poll = fun ?max_events ?timeout t -> Adapter.Selector.select ?timeout ?max_events t.selector

let register = fun (t:t) token interests source ->
  Source.register source t.selector token interests

let reregister = fun (t:t) token interests source ->
  Source.reregister source t.selector token interests

let deregister = fun (t:t) source ->
  Source.deregister source t.selector
