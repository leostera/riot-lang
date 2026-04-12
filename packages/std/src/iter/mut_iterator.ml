open Kernel

module type Intf = sig
  type state
  type item
  val next: state -> item option

  val size: state -> int

  val clone: state -> state
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)

type 'item t =
  Iter: (('item, 'state) iter * 'state) -> 'item t

let make: type item state. (item, state) iter -> state -> item t = fun mod_ state ->
  Iter (mod_, state)

let empty = fun (type value) () ->
  let module Empty = struct
    type item = value

    type state = unit

    let next = fun () -> None

    let size = fun () -> 0

    let clone = fun () -> ()
  end in
  make (module Empty) ()

let singleton = fun (type value) (item: value) ->
  let module Singleton = struct
    type item = value

    type state = {
      mutable value: value option;
    }

    let next = fun state ->
      match state.value with
      | Some value ->
          state.value <- None;
          Some value
      | None -> None

    let size = fun state ->
      match state.value with
      | Some _ -> 1
      | None -> 0

    let clone = fun state -> { value = state.value }
  end in
  make (module Singleton) { value = Some item }

let next: type item. item t -> item option = fun (Iter ((module Iter), state)) -> Iter.next state

let size: type item. item t -> int = fun (Iter ((module Iter), state)) -> Iter.size state

let clone: type item. item t -> item t = fun (Iter ((module Iter), state)) ->
  let state = Iter.clone state in
  Iter ((module Iter), state)

let rec collect = fun iter acc ->
  match next iter with
  | Some item -> collect iter (item :: acc)
  | None -> List.rev acc

let to_list = fun iter -> collect iter []

let iter_next = next

let map: type a b. a t -> fn:(a -> b) -> b t = fun iter ~fn ->
  let module MapIter = struct
    type state = a t

    type item = b

    let next = fun state ->
      Option.map fn (iter_next state)

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module MapIter) iter

let filter: type a. a t -> fn:(a -> bool) -> a t = fun iter ~fn ->
  let module FilterIter = struct
    type state = a t

    type item = a

    let rec next = fun state ->
      match iter_next state with
      | Some value when fn value -> Some value
      | Some _ -> next state
      | None -> None

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module FilterIter) iter

let filter_map: type a b. a t -> fn:(a -> b option) -> b t = fun iter ~fn ->
  let module FilterMapIter = struct
    type state = a t

    type item = b

    let rec next = fun state ->
      match iter_next state with
      | Some value -> (
          match fn value with
          | Some mapped -> Some mapped
          | None -> next state
        )
      | None -> None

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module FilterMapIter) iter

let flat_map: type a b. a t -> fn:(a -> b t) -> b t = fun iter ~fn ->
  let module FlatMapIter = struct
    type state = {
      outer: a t;
      mutable current: b t option;
    }

    type item = b

    let rec next = fun state ->
      match state.current with
      | Some inner -> (
          match iter_next inner with
          | Some value -> Some value
          | None ->
              state.current <- None;
              next state
        )
      | None -> (
          match iter_next state.outer with
          | Some value ->
              state.current <- Some (fn value);
              next state
          | None -> None
        )

    let size = fun state -> size state.outer

    let clone = fun state -> { outer = clone state.outer; current = Option.map clone state.current }
  end in
  make (module FlatMapIter) { outer = iter; current = None }

let fold: type a acc. a t -> init:acc -> fn:(a -> acc -> acc) -> acc = fun iter ~init ~fn ->
  let rec loop acc =
    match next iter with
    | Some value -> loop (fn value acc)
    | None -> acc
  in
  loop init

let reduce: type a. a t -> fn:(a -> a -> a) -> a option = fun iter ~fn ->
  match next iter with
  | Some first -> Some (fold iter ~init:first ~fn)
  | None -> None

let count: type a. a t -> int = fun iter -> fold iter ~init:0 ~fn:(fun _ count -> count + 1)

let find: type a. a t -> fn:(a -> bool) -> a option = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some value when fn value -> Some value
    | Some _ -> loop ()
    | None -> None
  in
  loop ()

let any: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some value when fn value -> true
    | Some _ -> loop ()
    | None -> false
  in
  loop ()

let all: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some value when fn value -> loop ()
    | Some _ -> false
    | None -> true
  in
  loop ()

let take: type a. a t -> int -> a t = fun iter count ->
  let module TakeIter = struct
    type state = {
      iter: a t;
      mutable remaining: int;
    }

    type item = a

    let next = fun state ->
      if state.remaining <= 0 then
        None
      else (
        state.remaining <- state.remaining - 1;
        iter_next state.iter
      )

    let size = fun state ->
      Int.min state.remaining (size state.iter)

    let clone = fun state -> { iter = clone state.iter; remaining = state.remaining }
  end in
  make (module TakeIter) { iter; remaining = count }

let drop: type a. a t -> int -> a t = fun iter count ->
  for _ = 1 to count do
    let _ = next iter in
    ()
  done;
  iter

let enumerate: type a. a t -> (int * a) t = fun iter ->
  let module EnumIter = struct
    type state = {
      iter: a t;
      mutable index: int;
    }

    type item = int * a

    let next = fun state ->
      match iter_next state.iter with
      | Some value ->
          let index = state.index in
          state.index <- state.index + 1;
          Some (index, value)
      | None -> None

    let size = fun state -> size state.iter

    let clone = fun state -> { iter = clone state.iter; index = state.index }
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
      match (iter_next state.left, iter_next state.right) with
      | Some left, Some right -> Some (left, right)
      | _ -> None

    let size = fun state ->
      Int.min (size state.left) (size state.right)

    let clone = fun state -> { left = clone state.left; right = clone state.right }
  end in
  make (module ZipIter) { left; right }

let chain: type a. a t -> a t -> a t = fun first second ->
  let module ChainIter = struct
    type state = {
      first: a t;
      second: a t;
      mutable in_first: bool;
    }

    type item = a

    let rec next = fun state ->
      if state.in_first then
        match iter_next state.first with
        | Some value -> Some value
        | None ->
            state.in_first <- false;
            next state
      else
        iter_next state.second

    let size = fun state ->
      if state.in_first then
        size state.first + size state.second
      else
        size state.second

    let clone = fun state ->
      { first = clone state.first; second = clone state.second; in_first = state.in_first }
  end in
  make (module ChainIter) { first; second; in_first = true }

let for_each: type a. a t -> fn:(a -> unit) -> unit = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some value ->
        fn value;
        loop ()
    | None -> ()
  in
  loop ()
