open Kernel

module Runtime_atomic = Kernel.Sync.Atomic

type t = int

let counter = Runtime_atomic.make (-1)

let main = 0

let next = fun () ->
  let rec next_id () =
    let current = Runtime_atomic.get counter in
    let next = current + 1 in
    if Runtime_atomic.compare_and_set counter current next then
      next
    else next_id ()
  in
  next_id ()

let equal = Int.equal

let compare = Int.compare

let to_int = fun t -> t

let to_string = fun t -> Format.format Format.[ str "pid<"; int t; char '>' ]
