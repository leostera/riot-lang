(** Re-export core helpers from Kernel *)
let format = Kernel.format
let print = Kernel.print
let println = Kernel.println

exception Deprecated

let failwith = Deprecated

(** Panic with backtrace - enhances Kernel.panic with backtrace info *)
let panic msg =
  let exception Panic of string in
  let bt = Exception.get_backtrace () in
  let msg = format "%s\nBacktrace: %s" msg bt in
  raise (Panic msg)

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
