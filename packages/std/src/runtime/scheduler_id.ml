open Kernel

type t =
  | Scheduler_id of int

let panic = Kernel.SystemError.panic

let zero = Scheduler_id 0

let from_int = fun value ->
  if value < 0 then
    panic "Scheduler_id.from_int expects a non-negative integer";
  Scheduler_id value

let to_int = fun (Scheduler_id value) -> value

let succ = fun (Scheduler_id value) -> Scheduler_id (value + 1)

let equal = fun (Scheduler_id a) (Scheduler_id b) -> Int.equal a b

let compare = fun (Scheduler_id a) (Scheduler_id b) -> Int.compare a b
