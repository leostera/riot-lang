(** Panic exception and function *)

(** Format string helper *)
let format = Kernel.format

exception Deprecated

let failwith = Deprecated

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

(** Print to stdout with flush *)
let print fmt =
  Kernel.Printf.ksprintf (fun s -> Kernel.Printf.printf "%s%!" s) fmt

(** Print to stdout with newline and flush *)
let println fmt =
  Kernel.Printf.ksprintf (fun s -> Kernel.Printf.printf "%s\n%!" s) fmt

let todo msg = panic (format "TODO: %s" msg)
let unimplemented () = panic "unimplemented"
