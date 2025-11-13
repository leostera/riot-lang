include Kernel.Types
include Kernel.Global

(** Process management globals *)
include Miniriot.Exception

type 'msg selector = 'msg Miniriot.selector

let self = Miniriot.self

let spawn = Miniriot.spawn

let spawn_link = Miniriot.spawn_link

let send = Miniriot.send

let receive ~selector ?timeout () = 
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  Miniriot.receive ~selector ?timeout ()
  
let receive_any ?timeout () =
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  Miniriot.receive_any ?timeout ()

let sleep timeout =
  let selector _msg = `skip in
  try receive ~selector ~timeout () 
  with Receive_timeout -> ()

let yield = Miniriot.yield

let shutdown = Miniriot.shutdown

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
let panic msg =
  let exception Panic of string in
  Kernel.raise (Panic msg)

(** Create a mutable cell *)
let cell x = Kernel.Sync.Cell.create x

let ref = cell

(** Cell operators for ref-like syntax *)
let ( ! ) = Kernel.Sync.Cell.get

let ( := ) = Kernel.Sync.Cell.set

let todo msg = panic ("TODO: " ^ msg)
let unimplemented () = panic "unimplemented"

