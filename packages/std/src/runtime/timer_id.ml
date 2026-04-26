open Kernel

module Runtime_atomic = Kernel.Sync.Atomic

type t = int64

let next_id = Runtime_atomic.make 0L

let make (): t =
  let rec try_increment () =
    let current = Runtime_atomic.get next_id in
    let next = Int64.add current 1L in
    if Runtime_atomic.compare_and_set next_id current next then
      next
    else
      try_increment ()
  in
  try_increment ()

let equal: t -> t -> bool = fun a b -> Int64.equal a b

let compare: t -> t -> Order.t = fun a b -> Int64.compare a b
