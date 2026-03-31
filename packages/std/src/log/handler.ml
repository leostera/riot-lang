open Global
open Collections

(** Handler system - handlers are called directly in caller process *)
type id = string

type t = {
  id: id;
  fn: Event.t -> unit;
}

let handlers : (id, t) HashMap.t = HashMap.create ()

(** Emit event to all handlers - called directly in caller process *)
let emit = fun event ->
    (* Call each handler, catching and ignoring exceptions *)
    HashMap.iter
      (fun _id handler ->
        try handler.fn event with
        | _ -> ())
      handlers

(** Attach a handler *)
let attach = fun id fn -> HashMap.insert handlers id {id; fn} |> ignore

(** Detach a handler by ID *)
let detach = fun id -> HashMap.remove handlers id |> ignore

(** Detach all handlers *)
let detach_all = fun () -> HashMap.clear handlers

(** List all handler IDs *)
let list = fun () -> HashMap.keys handlers
