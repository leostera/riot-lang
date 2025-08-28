module type Intf = sig
  type state
  type item

  val next : state -> item option
  val size : state -> int
  val clone : state -> state
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)

type 'item t = Iter : (('item, 'state) iter * 'state) -> 'item t

(*************************************************************************************************)

let make : type item state. (item, state) iter -> state -> item t =
 fun mod_ state -> Iter (mod_, state)

let next : type item. item t -> item option =
 fun (Iter ((module Iter), state)) -> Iter.next state

let size : type item. item t -> int =
 fun (Iter ((module Iter), state)) -> Iter.size state

let clone : type item. item t -> item t =
 fun (Iter ((module Iter), state)) ->
  let new_state = Iter.clone state in
  Iter ((module Iter), new_state)

(*************************************************************************************************)

let rec collect t acc =
  match next t with
  | Some item -> collect t (item :: acc)
  | None -> List.rev acc

let to_list t = collect t []
