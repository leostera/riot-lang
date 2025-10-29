open Kernel

type t = int64

let next_id = Atomic.make 0L

let make () : t =
  let rec try_increment () =
    let current = Atomic.get next_id in
    let next = Int64.add current 1L in
    if Atomic.compare_and_set next_id current next then next
    else try_increment ()
  in
  try_increment ()

let equal (a : t) (b : t) : bool = Int64.equal a b
let compare (a : t) (b : t) : int = Int64.compare a b
let pp fmt (id : t) = Format.fprintf fmt "timer<%Ld>" id
