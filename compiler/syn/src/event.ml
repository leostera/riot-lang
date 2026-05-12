open Std
open Std.Collections

type t =
  | StartNode of Syntax_kind.t option
  | FinishNode
  | Token of int
  | Missing of Syntax_kind.t * int
  | Error of Diagnostic.t

type event = t

module Buffer = struct
  type event = t

  type t = {
    events: event Vector.t;
    diagnostics: Diagnostic.t Vector.t;
  }

  type marker = { index: int }

  type completed = {
    start_index: int;
    kind: Syntax_kind.t;
  }

  let create = fun ?(event_capacity = 0) ?(diagnostic_capacity = 0) () -> {
    events = Vector.with_capacity ~size:event_capacity;
    diagnostics = Vector.with_capacity ~size:diagnostic_capacity;
  }

  let start_node = fun t ->
    let index = Vector.length t.events in
    Vector.push t.events ~value:(StartNode None);
    { index }

  let complete = fun t marker kind ->
    Vector.set_unchecked t.events ~at:marker.index ~value:(StartNode (Some kind));
    Vector.push t.events ~value:FinishNode;
    { start_index = marker.index; kind }

  let precede = fun t completed ->
    Vector.insert t.events ~at:completed.start_index ~value:(StartNode None);
    { index = completed.start_index }

  let token = fun t ~raw_index -> Vector.push t.events ~value:(Token raw_index)

  let missing = fun t ~kind ~offset -> Vector.push t.events ~value:(Missing (kind, offset))

  let error = fun t diagnostic ->
    Vector.push t.diagnostics ~value:diagnostic;
    Vector.push t.events ~value:(Error diagnostic)

  let length = fun t -> Vector.length t.events

  let get_unchecked = fun t ~at -> Vector.get_unchecked t.events ~at

  let iter = fun t -> Vector.iter t.events

  let diagnostics = fun t -> t.diagnostics
end
