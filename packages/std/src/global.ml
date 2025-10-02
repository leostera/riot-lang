(** Panic exception and function *)

exception Deprecated

let failwith = Deprecated

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(** Create a mutable cell *)
let cell x = Cell.create x

(** Format string helper *)
let format = Kernel.format

(** Print to stdout with flush *)
let print fmt = Kernel.Printf.ksprintf (fun s -> Kernel.Printf.printf "%s%!" s) fmt

(** Print to stdout with newline and flush *)
let println fmt = Kernel.Printf.ksprintf (fun s -> Kernel.Printf.printf "%s\n%!" s) fmt
