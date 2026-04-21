open Std
open Std.Collections

type t =
  | StartNode of Syntax_kind2.t option
  | FinishNode
  | Token of int
  | Missing of Syntax_kind2.t * int
  | Error of Diagnostic.t

module Buffer = struct
  type buffer = t

  type t = {
    events: buffer Vector.t;
    diagnostics: Diagnostic.t Vector.t;
  }

  type marker = {
    index: int;
  }

  type completed = {
    start_index: int;
    kind: Syntax_kind2.t;
  }

  let create = fun () -> { events = Vector.create (); diagnostics = Vector.create () }

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

  let to_array = fun t -> Vector.to_array t.events

  let diagnostics = fun t -> Vector.to_array t.diagnostics |> Array.to_list
end
