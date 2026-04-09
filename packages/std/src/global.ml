include Kernel.Types

include Kernel.Global

(** Process management globals *)
include Runtime.Exception

type 'msg selector = 'msg Runtime.selector

let self = Runtime.self

let spawn = Runtime.spawn

let spawn_link = Runtime.spawn_link

let send = Runtime.send

let receive = fun ~selector ?timeout () ->
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  Runtime.receive ~selector ?timeout ()

let receive_any = fun ?timeout () ->
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  Runtime.receive_any ?timeout ()

let sleep = fun timeout ->
  let selector _msg = `skip in
  try receive ~selector ~timeout () with
  | Receive_timeout -> ()

let yield = Runtime.yield

let shutdown = Runtime.shutdown

open Kernel

(** Re-export core helpers from Kernel - print/println are now async-safe *)
let print = Kernel.print

let println = Kernel.println

let eprint = Kernel.eprint

let eprintln = Kernel.eprintln

(** Collection type aliases and constructors from Kernel *)
type 'a vec = 'a Kernel.vec

type 'a queue = 'a Kernel.queue

type 'a set = 'a Kernel.set

type ('k, 'v) map = ('k, 'v) Kernel.map

let vec = Kernel.vec

let queue = Kernel.queue

let set = Kernel.set

let map = Kernel.map

(** Panic with a message and backtrace *)
let panic = fun msg ->
  let exception Panic of string in
  Kernel.raise (Panic msg)

(** Create a mutable cell *)
let cell = fun x -> Kernel.Sync.Cell.create x

let ref = cell

(** Cell operators for ref-like syntax *)
let ( ! ) = Kernel.Sync.Cell.get

let ( := ) = Kernel.Sync.Cell.set

let todo = fun msg -> panic (format Format.[ str "TODO: "; str msg ])

let unimplemented = fun () -> panic "unimplemented"
