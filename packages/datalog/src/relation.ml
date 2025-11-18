open Std

(** A relation is a sorted, deduplicated iterator.
    
    INVARIANT: The iterator MUST yield elements in sorted order.
    Violating this invariant will cause incorrect results from set operations.
*)
type 'a t = 'a Iter.MutIterator.t

(* Internal helpers for lazy set operations *)

let dedup_sorted (type a) (iter : a Iter.MutIterator.t) : a Iter.MutIterator.t =
  let module DedupIter = struct
    type state = { iter : a Iter.MutIterator.t; mutable last : a option }
    type item = a

    let rec next state =
      match Iter.MutIterator.next state.iter with
      | None -> None
      | Some x -> (
          match state.last with
          | Some prev when compare x prev = 0 -> next state  (* Skip duplicate *)
          | _ -> state.last <- Some x; Some x)

    let size state = Iter.MutIterator.size state.iter
    let clone state = { iter = Iter.MutIterator.clone state.iter; last = state.last }
  end in
  Iter.MutIterator.make (module DedupIter) { iter; last = None }

let merge_sorted (type a) (iter1 : a Iter.MutIterator.t) (iter2 : a Iter.MutIterator.t) : a Iter.MutIterator.t =
  let module MergeIter = struct
    type state = { 
      iter1 : a Iter.MutIterator.t; 
      iter2 : a Iter.MutIterator.t;
      mutable peek1 : a option;
      mutable peek2 : a option;
    }
    type item = a

    let next state =
      match (state.peek1, state.peek2) with
      | None, None -> None
      | Some x, None -> 
          state.peek1 <- Iter.MutIterator.next state.iter1;
          Some x
      | None, Some y ->
          state.peek2 <- Iter.MutIterator.next state.iter2;
          Some y
      | Some x, Some y ->
          match compare x y with
          | c when c < 0 -> 
              state.peek1 <- Iter.MutIterator.next state.iter1;
              Some x
          | c when c > 0 -> 
              state.peek2 <- Iter.MutIterator.next state.iter2;
              Some y
          | _ -> (* Equal - yield once, advance both *)
              state.peek1 <- Iter.MutIterator.next state.iter1;
              state.peek2 <- Iter.MutIterator.next state.iter2;
              Some x

    let size state = Iter.MutIterator.size state.iter1 + Iter.MutIterator.size state.iter2
    let clone state = { 
      iter1 = Iter.MutIterator.clone state.iter1; 
      iter2 = Iter.MutIterator.clone state.iter2;
      peek1 = state.peek1;
      peek2 = state.peek2;
    }
  end in
  Iter.MutIterator.make (module MergeIter) { 
    iter1; 
    iter2; 
    peek1 = Iter.MutIterator.next iter1;
    peek2 = Iter.MutIterator.next iter2;
  }

let diff_sorted (type a) (iter1 : a Iter.MutIterator.t) (iter2 : a Iter.MutIterator.t) : a Iter.MutIterator.t =
  let module DiffIter = struct
    type state = { 
      iter1 : a Iter.MutIterator.t; 
      iter2 : a Iter.MutIterator.t;
      mutable peek1 : a option;
      mutable peek2 : a option;
    }
    type item = a

    let rec next state =
      match (state.peek1, state.peek2) with
      | None, _ -> None
      | Some x, None -> 
          state.peek1 <- Iter.MutIterator.next state.iter1;
          Some x
      | Some x, Some y ->
          match compare x y with
          | c when c < 0 -> 
              state.peek1 <- Iter.MutIterator.next state.iter1;
              Some x
          | c when c > 0 -> 
              state.peek2 <- Iter.MutIterator.next state.iter2;
              next state
          | _ -> (* Equal - skip both *)
              state.peek1 <- Iter.MutIterator.next state.iter1;
              state.peek2 <- Iter.MutIterator.next state.iter2;
              next state

    let size state = Iter.MutIterator.size state.iter1
    let clone state = { 
      iter1 = Iter.MutIterator.clone state.iter1; 
      iter2 = Iter.MutIterator.clone state.iter2;
      peek1 = state.peek1;
      peek2 = state.peek2;
    }
  end in
  Iter.MutIterator.make (module DiffIter) { 
    iter1; 
    iter2; 
    peek1 = Iter.MutIterator.next iter1;
    peek2 = Iter.MutIterator.next iter2;
  }

let intersect_sorted (type a) (iter1 : a Iter.MutIterator.t) (iter2 : a Iter.MutIterator.t) : a Iter.MutIterator.t =
  let module IntersectIter = struct
    type state = { 
      iter1 : a Iter.MutIterator.t; 
      iter2 : a Iter.MutIterator.t;
      mutable peek1 : a option;
      mutable peek2 : a option;
    }
    type item = a

    let rec next state =
      match (state.peek1, state.peek2) with
      | None, _ | _, None -> None
      | Some x, Some y ->
          match compare x y with
          | c when c < 0 -> 
              state.peek1 <- Iter.MutIterator.next state.iter1;
              next state
          | c when c > 0 -> 
              state.peek2 <- Iter.MutIterator.next state.iter2;
              next state
          | _ -> (* Equal - found in both! *)
              state.peek1 <- Iter.MutIterator.next state.iter1;
              state.peek2 <- Iter.MutIterator.next state.iter2;
              Some x

    let size state = min (Iter.MutIterator.size state.iter1) (Iter.MutIterator.size state.iter2)
    let clone state = { 
      iter1 = Iter.MutIterator.clone state.iter1; 
      iter2 = Iter.MutIterator.clone state.iter2;
      peek1 = state.peek1;
      peek2 = state.peek2;
    }
  end in
  Iter.MutIterator.make (module IntersectIter) { 
    iter1; 
    iter2; 
    peek1 = Iter.MutIterator.next iter1;
    peek2 = Iter.MutIterator.next iter2;
  }

(* Public API *)

let empty (type a) () : a Iter.MutIterator.t =
  let module EmptyIter = struct
    type state = unit
    type item = a
    let next _ = None
    let size _ = 0
    let clone _ = ()
  end in
  Iter.MutIterator.make (module EmptyIter) ()

let of_iter iter = dedup_sorted iter

let of_list (type a) (xs : a list) : a Iter.MutIterator.t =
  let sorted = List.sort compare xs in
  let module ListIter = struct
    type state = { mutable remaining : a list }
    type item = a
    let next state = 
      match state.remaining with
      | [] -> None
      | x :: xs -> state.remaining <- xs; Some x
    let size state = List.length state.remaining
    let clone state = { remaining = state.remaining }
  end in
  let iter = Iter.MutIterator.make (module ListIter) { remaining = sorted } in
  dedup_sorted iter

let singleton (type a) (x : a) : a Iter.MutIterator.t =
  let module SingletonIter = struct
    type state = { mutable value : a option }
    type item = a
    let next state = 
      match state.value with
      | None -> None
      | Some v -> state.value <- None; Some v
    let size state = match state.value with None -> 0 | Some _ -> 1
    let clone state = { value = state.value }
  end in
  Iter.MutIterator.make (module SingletonIter) { value = Some x }

let to_list = Iter.MutIterator.to_list

let length rel = 
  let count = ref 0 in
  Iter.MutIterator.for_each rel ~fn:(fun _ -> count := !count + 1);
  !count

let is_empty rel = 
  match Iter.MutIterator.next rel with 
  | None -> true 
  | Some _ -> false

let merge = merge_sorted
let diff = diff_sorted
let intersect = intersect_sorted

let iter f rel = Iter.MutIterator.for_each rel ~fn:f

let fold f acc rel = 
  let result = ref acc in
  Iter.MutIterator.for_each rel ~fn:(fun x -> result := f !result x);
  !result

let map f rel = 
  Iter.MutIterator.map rel ~fn:f
  (* WARNING: Mapping may break sort order! Caller must ensure f preserves order,
     or re-sort after mapping *)

let filter f rel = Iter.MutIterator.filter rel ~fn:f

let contains rel x =
  let rec search () =
    match Iter.MutIterator.next rel with
    | None -> false
    | Some y ->
        match compare x y with
        | 0 -> true
        | c when c < 0 -> false  (* Passed x in sorted sequence *)
        | _ -> search ()
  in
  search ()

let find f rel =
  let rec search () =
    match Iter.MutIterator.next rel with
    | None -> None
    | Some x -> if f x then Some x else search ()
  in
  search ()
