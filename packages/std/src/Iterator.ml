module type Intf = sig
  type state
  type item

  val next : state -> item option * state
  val size : state -> int
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)

type 'item t = Iter : (('item, 'state) iter * 'state) -> 'item t

(*************************************************************************************************)

let[@inline] make : type item state. (item, state) iter -> state -> item t =
 fun mod_ state -> Iter (mod_, state)

let[@inline] next : type item. item t -> item option * item t =
 fun (Iter (((module Iter) as mod_), state)) ->
  let item, state' = Iter.next state in
  (item, Iter (mod_, state'))

let[@inline] size : type item. item t -> int =
 fun (Iter ((module Iter), state)) -> Iter.size state

(*************************************************************************************************)

let rec collect t acc =
  match next t with
  | Some item, t -> collect t (item :: acc)
  | None, _ -> List.rev acc

let to_list t = collect t []
