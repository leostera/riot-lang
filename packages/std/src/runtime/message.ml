open Kernel
module Runtime_atomic = Kernel.Sync.Atomic

type t = ..

type envelope = {
  msg: t;
  uid: int;
}

let uid_counter = Runtime_atomic.make 0

let envelope = fun msg ->
  let rec next_id () =
    let current = Runtime_atomic.get uid_counter in
    let next = current + 1 in
    if Runtime_atomic.compare_and_set uid_counter current next then
      next
    else
      next_id ()
  in
  { msg; uid = next_id () }
