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

(*************************************************************************************************)
(* Transformation *)
(*************************************************************************************************)

let iter_next = next

let map (type a b) (iter : a t) ~(fn : a -> b) : b t =
  let module MapIter = struct
    type state = a t
    type item = b

    let next state = Option.map fn (iter_next state)
    let size state = size state
    let clone state = clone state
  end in
  make (module MapIter) iter

let filter (type a) (iter : a t) ~(fn : a -> bool) : a t =
  let module FilterIter = struct
    type state = a t
    type item = a

    let rec next state =
      match iter_next state with
      | Some x when fn x -> Some x
      | Some _ -> next state
      | None -> None

    let size state = size state
    let clone state = clone state
  end in
  make (module FilterIter) iter

let filter_map (type a b) (iter : a t) ~(fn : a -> b option) : b t =
  let module FilterMapIter = struct
    type state = a t
    type item = b

    let rec next state =
      match iter_next state with
      | Some x -> (match fn x with Some y -> Some y | None -> next state)
      | None -> None

    let size state = size state
    let clone state = clone state
  end in
  make (module FilterMapIter) iter

(*************************************************************************************************)
(* Reduction *)
(*************************************************************************************************)

let fold (type a acc) (iter : a t) ~(init : acc) ~(fn : a -> acc -> acc) : acc
    =
  let rec loop acc =
    match next iter with Some x -> loop (fn x acc) | None -> acc
  in
  loop init

let reduce (type a) (iter : a t) ~(fn : a -> a -> a) : a option =
  match next iter with Some first -> Some (fold iter ~init:first ~fn) | None -> None

let count (type a) (iter : a t) : int =
  fold iter ~init:0 ~fn:(fun _ acc -> acc + 1)

(*************************************************************************************************)
(* Search *)
(*************************************************************************************************)

let find (type a) (iter : a t) ~(fn : a -> bool) : a option =
  let rec loop () =
    match next iter with
    | Some x when fn x -> Some x
    | Some _ -> loop ()
    | None -> None
  in
  loop ()

let any (type a) (iter : a t) ~(fn : a -> bool) : bool =
  let rec loop () =
    match next iter with
    | Some x when fn x -> true
    | Some _ -> loop ()
    | None -> false
  in
  loop ()

let all (type a) (iter : a t) ~(fn : a -> bool) : bool =
  let rec loop () =
    match next iter with
    | Some x when fn x -> loop ()
    | Some _ -> false
    | None -> true
  in
  loop ()

(*************************************************************************************************)
(* Combinators *)
(*************************************************************************************************)

let take (type a) (iter : a t) (n : int) : a t =
  let module TakeIter = struct
    type state = { iter : a t; mutable remaining : int }
    type item = a

    let next state =
      if state.remaining <= 0 then None
      else (
        state.remaining <- state.remaining - 1;
        iter_next state.iter)

    let size state = min state.remaining (size state.iter)
    let clone state = { iter = clone state.iter; remaining = state.remaining }
  end in
  make (module TakeIter) { iter; remaining = n }

let drop (type a) (iter : a t) (n : int) : a t =
  for _ = 1 to n do
    ignore (next iter)
  done;
  iter

let enumerate (type a) (iter : a t) : (int * a) t =
  let module EnumIter = struct
    type state = { iter : a t; mutable index : int }
    type item = int * a

    let next state =
      match iter_next state.iter with
      | Some x ->
          let idx = state.index in
          state.index <- state.index + 1;
          Some (idx, x)
      | None -> None

    let size state = size state.iter
    let clone state = { iter = clone state.iter; index = state.index }
  end in
  make (module EnumIter) { iter; index = 0 }

let zip (type a b) (iter1 : a t) (iter2 : b t) : (a * b) t =
  let module ZipIter = struct
    type state = { iter1 : a t; iter2 : b t }
    type item = a * b

    let next state =
      match (iter_next state.iter1, iter_next state.iter2) with
      | Some x, Some y -> Some (x, y)
      | _ -> None

    let size state = min (size state.iter1) (size state.iter2)
    let clone state = { iter1 = clone state.iter1; iter2 = clone state.iter2 }
  end in
  make (module ZipIter) { iter1; iter2 }

let chain (type a) (iter1 : a t) (iter2 : a t) : a t =
  let module ChainIter = struct
    type state = { first : a t; second : a t; mutable in_first : bool }
    type item = a

    let rec next state =
      if state.in_first then
        match iter_next state.first with
        | Some x -> Some x
        | None ->
            state.in_first <- false;
            next state
      else iter_next state.second

    let size state =
      if state.in_first then size state.first + size state.second
      else size state.second

    let clone state =
      {
        first = clone state.first;
        second = clone state.second;
        in_first = state.in_first;
      }
  end in
  make (module ChainIter) { first = iter1; second = iter2; in_first = true }

(*************************************************************************************************)
(* Side Effects *)
(*************************************************************************************************)

let for_each (type a) (iter : a t) ~(fn : a -> unit) : unit =
  let rec loop () = match next iter with Some x -> fn x; loop () | None -> () in
  loop ()
