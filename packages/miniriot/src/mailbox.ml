open Kernel
open Kernel.Collections

type t = { mutable queue : Message.envelope Queue.t; mutable size : int }

let create () = { queue = Queue.create (); size = 0 }

let queue t msg =
  Queue.push t.queue msg;
  t.size <- t.size + 1

let next t =
  match Queue.pop t.queue with
  | None -> None
  | Some msg ->
    t.size <- t.size - 1;
    Some msg

let size t = t.size
let is_empty t = Queue.is_empty t.queue
