open Global
open Collections

(** Handler system - handlers are called directly in caller process *)
type id = string

type t = {
  id: id;
  fn: Event.t -> unit;
}

let handlers: (id, t) HashMap.t = HashMap.create ()

(** Emit event to all handlers - called directly in caller process *)
let emit = fun event ->
  (* Call each handler, catching and ignoring exceptions *)
  HashMap.for_each handlers
    ~fn:(fun _id handler ->
      try handler.fn event with
      | _ -> ())

let attach = fun id fn ->
  (* keep inserted value only for side-effects on map state *)
  let _ = HashMap.insert handlers ~key:id ~value:{ id; fn } in
  ()

let detach = fun id ->
  let _ = HashMap.remove handlers ~key:id in
  ()

(** Detach all handlers *)
let detach_all = fun () -> HashMap.clear handlers

(** List all handler IDs *)
let list = fun () -> HashMap.keys handlers
