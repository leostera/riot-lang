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

(*************************************************************************************************)
(* Transformation *)
(*************************************************************************************************)

(* Bind next locally to avoid module recursion *)
let iter_next = next

let map (type a b) (iter : a t) ~(fn : a -> b) : b t =
  let module MapIter = struct
    type state = a t
    type item = b

    let next state =
      match iter_next state with
      | Some x, state' -> (Some (fn x), state')
      | None, state' -> (None, state')

    let size state = size state
  end in
  make (module MapIter) iter

let filter (type a) (iter : a t) ~(fn : a -> bool) : a t =
  let module FilterIter = struct
    type state = a t
    type item = a

    let rec next_filtered state =
      match iter_next state with
      | Some x, state' when fn x -> (Some x, state')
      | Some _, state' -> next_filtered state'
      | None, state' -> (None, state')
    
    let next = next_filtered

    let size state = size state
  end in
  make (module FilterIter : Intf with type state = a t and type item = a) iter

let filter_map (type a b) (iter : a t) ~(fn : a -> b option) : b t =
  let module FilterMapIter = struct
    type state = a t
    type item = b

    let rec next_filtered state =
      match iter_next state with
      | Some x, state' -> (
          match fn x with Some y -> (Some y, state') | None -> next_filtered state')
      | None, state' -> (None, state')
    
    let next = next_filtered

    let size state = size state
  end in
  make
    (module FilterMapIter : Intf with type state = a t and type item = b)
    iter

(*************************************************************************************************)
(* Reduction *)
(*************************************************************************************************)

let fold (type a acc) (iter : a t) ~(init : acc) ~(fn : a -> acc -> acc) : acc
    =
  let rec loop t acc =
    match next t with
    | Some x, t' -> loop t' (fn x acc)
    | None, _ -> acc
  in
  loop iter init

let reduce (type a) (iter : a t) ~(fn : a -> a -> a) : a option =
  match next iter with
  | Some first, iter' -> Some (fold iter' ~init:first ~fn)
  | None, _ -> None

let count (type a) (iter : a t) : int =
  fold iter ~init:0 ~fn:(fun _ acc -> acc + 1)

(*************************************************************************************************)
(* Search *)
(*************************************************************************************************)

let find (type a) (iter : a t) ~(fn : a -> bool) : a option =
  let rec loop t =
    match next t with
    | Some x, t' when fn x -> Some x
    | Some _, t' -> loop t'
    | None, _ -> None
  in
  loop iter

let any (type a) (iter : a t) ~(fn : a -> bool) : bool =
  let rec loop t =
    match next t with
    | Some x, _ when fn x -> true
    | Some _, t' -> loop t'
    | None, _ -> false
  in
  loop iter

let all (type a) (iter : a t) ~(fn : a -> bool) : bool =
  let rec loop t =
    match next t with
    | Some x, t' when fn x -> loop t'
    | Some _, _ -> false
    | None, _ -> true
  in
  loop iter

(*************************************************************************************************)
(* Combinators *)
(*************************************************************************************************)

let take (type a) (iter : a t) (n : int) : a t =
  let module TakeIter = struct
    type state = { iter : a t; remaining : int }
    type item = a

    let next state =
      if state.remaining <= 0 then (None, state)
      else
        match iter_next state.iter with
        | Some x, iter' ->
            (Some x, { iter = iter'; remaining = state.remaining - 1 })
        | None, iter' -> (None, { state with iter = iter' })

    let size state = min state.remaining (size state.iter)
  end in
  make (module TakeIter) { iter; remaining = n }

let drop (type a) (iter : a t) (n : int) : a t =
  let rec skip t count =
    if count <= 0 then t
    else match next t with _, t' -> skip t' (count - 1)
  in
  skip iter n

let enumerate (type a) (iter : a t) : (int * a) t =
  let module EnumIter = struct
    type state = { iter : a t; index : int }
    type item = int * a

    let next state =
      let item, iter' = iter_next state.iter in
      match item with
      | Some x -> (Some (state.index, x), { iter = iter'; index = state.index + 1 })
      | None -> (None, { state with iter = iter' })

    let size state = size state.iter
  end in
  make (module EnumIter) { iter; index = 0 }

let zip (type a b) (iter1 : a t) (iter2 : b t) : (a * b) t =
  let module ZipIter = struct
    type state = { iter1 : a t; iter2 : b t }
    type item = a * b

    let next state =
      let x_opt, iter1' = iter_next state.iter1 in
      let y_opt, iter2' = iter_next state.iter2 in
      match (x_opt, y_opt) with
      | Some x, Some y -> (Some (x, y), { iter1 = iter1'; iter2 = iter2' })
      | _ -> (None, { iter1 = iter1'; iter2 = iter2' })

    let size state = min (size state.iter1) (size state.iter2)
  end in
  make (module ZipIter) { iter1; iter2 }

let chain (type a) (iter1 : a t) (iter2 : a t) : a t =
  let module ChainIter = struct
    type state = { first : a t; second : a t; in_first : bool }
    type item = a

    let rec next_chain state =
      if state.in_first then
        let item, first' = iter_next state.first in
        match item with
        | Some x -> (Some x, { state with first = first' })
        | None -> next_chain { state with in_first = false }
      else
        let item, second' = iter_next state.second in
        match item with
        | Some x -> (Some x, { state with second = second' })
        | None -> (None, { state with second = second' })
    
    let next = next_chain

    let size state =
      if state.in_first then size state.first + size state.second
      else size state.second
  end in
  make (module ChainIter) { first = iter1; second = iter2; in_first = true }

(*************************************************************************************************)
(* Side Effects *)
(*************************************************************************************************)

let for_each (type a) (iter : a t) ~(fn : a -> unit) : unit =
  let rec loop t =
    match next t with
    | Some x, t' ->
        fn x;
        loop t'
    | None, _ -> ()
  in
  loop iter
