open Kernel

module type Intf = sig
  type state
  type item
  val next: state -> item option * state

  val size: state -> int
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)

type 'item t =
  Iter: (('item, 'state) iter * 'state) -> 'item t

let make: type item state. (item, state) iter -> state -> item t = fun mod_ state ->
  Iter (mod_, state)

let next: type item. item t -> item option * item t = fun (Iter (((module Iter) as mod_), state)) ->
  let item, state' = Iter.next state in
  (item, Iter (mod_, state'))

let size: type item. item t -> int = fun (Iter ((module Iter), state)) -> Iter.size state

let rec collect = fun iter acc ->
  match next iter with
  | Some item, iter -> collect iter (item :: acc)
  | None, _ -> List.rev acc

let to_list = fun iter -> collect iter []

let iter_next = next

let map: type a b. a t -> fn:(a -> b) -> b t = fun iter ~fn ->
  let module MapIter = struct
    type state = a t

    type item = b

    let next = fun state ->
      match iter_next state with
      | Some value, state -> (Some (fn value), state)
      | None, state -> (None, state)

    let size = fun state -> size state
  end in
  make (module MapIter) iter

let filter: type a. a t -> fn:(a -> bool) -> a t = fun iter ~fn ->
  let module FilterIter = struct
    type state = a t

    type item = a

    let rec next_filtered = fun state ->
      match iter_next state with
      | Some value, state when fn value -> (Some value, state)
      | Some _, state -> next_filtered state
      | None, state -> (None, state)

    let next = next_filtered

    let size = fun state -> size state
  end in
  make (module FilterIter : Intf with type state = a t and type item = a) iter

let filter_map: type a b. a t -> fn:(a -> b option) -> b t = fun iter ~fn ->
  let module FilterMapIter = struct
    type state = a t

    type item = b

    let rec next_filtered = fun state ->
      match iter_next state with
      | Some value, state -> (
          match fn value with
          | Some mapped -> (Some mapped, state)
          | None -> next_filtered state
        )
      | None, state -> (None, state)

    let next = next_filtered

    let size = fun state -> size state
  end in
  make (module FilterMapIter : Intf with type state = a t and type item = b) iter

let fold: type a acc. a t -> init:acc -> fn:(a -> acc -> acc) -> acc = fun iter ~init ~fn ->
  let rec loop iter acc =
    match next iter with
    | Some value, iter -> loop iter (fn value acc)
    | None, _ -> acc
  in
  loop iter init

let reduce: type a. a t -> fn:(a -> a -> a) -> a option = fun iter ~fn ->
  match next iter with
  | Some first, iter -> Some (fold iter ~init:first ~fn)
  | None, _ -> None

let count: type a. a t -> int = fun iter -> fold iter ~init:0 ~fn:(fun _ count -> count + 1)

let find: type a. a t -> fn:(a -> bool) -> a option = fun iter ~fn ->
  let rec loop iter =
    match next iter with
    | Some value, _ when fn value -> Some value
    | Some _, iter -> loop iter
    | None, _ -> None
  in
  loop iter

let any: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop iter =
    match next iter with
    | Some value, _ when fn value -> true
    | Some _, iter -> loop iter
    | None, _ -> false
  in
  loop iter

let all: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop iter =
    match next iter with
    | Some value, iter when fn value -> loop iter
    | Some _, _ -> false
    | None, _ -> true
  in
  loop iter

let take: type a. a t -> int -> a t = fun iter count ->
  let module TakeIter = struct
    type state = {
      iter: a t;
      remaining: int;
    }

    type item = a

    let next = fun state ->
      if state.remaining <= 0 then
        (None, state)
      else
        match iter_next state.iter with
        | Some value, iter -> (Some value, { iter; remaining = state.remaining - 1 })
        | None, iter -> (None, { state with iter })

    let size = fun state ->
      Int.min state.remaining (size state.iter)
  end in
  make (module TakeIter) { iter; remaining = count }

let drop: type a. a t -> int -> a t = fun iter count ->
  let rec skip iter count =
    if count <= 0 then
      iter
    else
      match next iter with
      | _, iter -> skip iter (count - 1)
  in
  skip iter count

let enumerate: type a. a t -> (int * a) t = fun iter ->
  let module EnumIter = struct
    type state = {
      iter: a t;
      index: int;
    }

    type item = int * a

    let next = fun state ->
      let item, iter = iter_next state.iter in
      match item with
      | Some value -> (Some (state.index, value), { iter; index = state.index + 1 })
      | None -> (None, { state with iter })

    let size = fun state -> size state.iter
  end in
  make (module EnumIter) { iter; index = 0 }

let zip: type a b. a t -> b t -> (a * b) t = fun left right ->
  let module ZipIter = struct
    type state = {
      left: a t;
      right: b t;
    }

    type item = a * b

    let next = fun state ->
      let left_value, left = iter_next state.left in
      let right_value, right = iter_next state.right in
      match (left_value, right_value) with
      | Some left_value, Some right_value -> (Some (left_value, right_value), { left; right })
      | _ -> (None, { left; right })

    let size = fun state ->
      Int.min (size state.left) (size state.right)
  end in
  make (module ZipIter) { left; right }

let chain: type a. a t -> a t -> a t = fun first second ->
  let module ChainIter = struct
    type state = {
      first: a t;
      second: a t;
      in_first: bool;
    }

    type item = a

    let rec next_chain = fun state ->
      if state.in_first then
        let item, first = iter_next state.first in
        match item with
        | Some value -> (Some value, { state with first })
        | None -> next_chain { state with in_first = false }
      else
        let item, second = iter_next state.second in
        match item with
        | Some value -> (Some value, { state with second })
        | None -> (None, { state with second })

    let next = next_chain

    let size = fun state ->
      if state.in_first then
        size state.first + size state.second
      else
        size state.second
  end in
  make (module ChainIter) { first; second; in_first = true }

let for_each: type a. a t -> fn:(a -> unit) -> unit = fun iter ~fn ->
  let rec loop iter =
    match next iter with
    | Some value, iter ->
        fn value;
        loop iter
    | None, _ -> ()
  in
  loop iter
