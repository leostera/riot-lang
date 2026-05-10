open Global

module Event = Event
module Span = Span

type event = Event.t = ..

type event +=
  | SpanEvent of Span.lifecycle

let emit = Supervisor.emit

let () = Span.set_emitter (fun event -> emit (SpanEvent event))

let with_span = fun ?span ?attributes name fn ->
  let span = Span.start ?span ?attributes name in
  try
    let result = fn span in
    Span.finish span;
    result
  with
  | exn ->
      Span.finish ~status:(Span.Failed exn) span;
      raise_notrace exn

let start = Supervisor.start

let attach = Supervisor.attach

let detach = Supervisor.detach

let detach_all = Supervisor.detach_all

let list_handlers = Supervisor.list_handlers

let stop = Supervisor.stop
