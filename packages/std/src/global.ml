(** Re-export core helpers from Kernel - print/println are now async-safe *)
let format = Kernel.format
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

exception Deprecated

let failwith = Deprecated

include Panic

(** Create a mutable cell *)
let cell x = Cell.create x

let ref = cell

(** Cell operators for ref-like syntax *)
let ( ! ) = Cell.get

let ( := ) = Cell.set

let todo msg = panic (format "TODO: %s" msg)
let unimplemented () = panic "unimplemented"

(** Process management globals *)
include Miniriot.Exception

type 'msg selector = 'msg Miniriot.selector
let self = Process.self
let spawn = Process.spawn
let spawn_link = Process.spawn_link
let send = Miniriot.send
let receive = Miniriot.receive
let receive_any = Miniriot.receive_any
let yield = Miniriot.yield
let shutdown = Miniriot.shutdown
