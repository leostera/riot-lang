open Kernel

external caml_hash: int -> int -> int -> 'value -> int = "caml_hash" [@@ noalloc]

type ('key, 'value) bucket = ('key * 'value) list Atomic.t

type ('key, 'value) bucket_slot = ('key, 'value) bucket option Atomic.t

type ('key, 'value) table = {
  buckets: ('key, 'value) bucket_slot array;
  sizes: int Atomic.t array;
}

type ('key, 'value) t = {
  capacity: int;
  table: ('key, 'value) table;
}

type ('key, 'value) entry =
  | Occupied of 'value
  | Vacant

type ('value, 'result) operation =
  | Insert of 'value * 'result
  | Remove of 'result
  | Abort of 'result

let minimum_capacity = 16

let max_size_counter_count = 64

let rec next_power_of_two = fun current target ->
  if current >= target then
    current
  else
    next_power_of_two (current * 2) target

let normalize_capacity = fun capacity -> next_power_of_two 1 (Int.max minimum_capacity capacity)

let default_capacity = fun () -> normalize_capacity (Thread.available_parallelism * 4)

let make_buckets = fun capacity ->
  Array.init
    ~count:capacity
    ~fn:(fun _ -> Atomic.make (None: ('key, 'value) bucket option))

let make_size_counters = fun capacity ->
  let count = Int.min max_size_counter_count capacity in
  Array.init ~count ~fn:(fun _ -> Atomic.make_contended 0)

let make_table = fun capacity -> {
  buckets = make_buckets capacity;
  sizes = make_size_counters capacity;
}

let make = fun capacity -> { capacity; table = make_table capacity }

let create = fun () -> make (default_capacity ())

let with_capacity = fun ~size -> make (normalize_capacity size)

let bucket_count = fun map -> map.capacity

let hash_index = fun map key ->
  let hash = caml_hash 10 100 0 key in
  (hash lsr 1) mod map.capacity

let bucket_slot_at = fun table index -> Array.get_unchecked table.buckets ~at:index

let size_counter_at = fun table bucket_index ->
  Array.get_unchecked
    table.sizes
    ~at:(bucket_index mod Array.length table.sizes)

let add_size = fun table bucket_index delta ->
  if not (Int.equal delta 0) then
    let _ = Atomic.fetch_and_add (size_counter_at table bucket_index) delta in
    ()

let make_bucket = fun bucket -> Atomic.make_contended bucket

let bucket_items = fun bucket_slot ->
  match Atomic.get bucket_slot with
  | None -> []
  | Some bucket_ref -> Atomic.get bucket_ref

let rec find_in_bucket = fun key bucket ->
  match bucket with
  | [] -> None
  | (existing_key, value) :: rest ->
      if existing_key = key then
        Some value
      else
        find_in_bucket key rest

let rec replace_in_bucket = fun key value bucket ->
  match bucket with
  | [] -> (None, [ (key, value); ], true)
  | (existing_key, existing_value) :: rest ->
      if existing_key = key then
        (Some existing_value, (key, value) :: rest, false)
      else
        let (previous, rest, inserted_new) = replace_in_bucket key value rest in
        (previous, (existing_key, existing_value) :: rest, inserted_new)

let rec remove_from_bucket = fun key bucket ->
  match bucket with
  | [] -> (None, [], false)
  | (existing_key, existing_value) :: rest ->
      if existing_key = key then
        (Some existing_value, rest, true)
      else
        let (removed, rest, did_remove) = remove_from_bucket key rest in
        (removed, (existing_key, existing_value) :: rest, did_remove)

let insert = fun map ~key ~value ->
  let table = map.table in
  let bucket_index = hash_index map key in
  let bucket_slot = bucket_slot_at table bucket_index in
  let rec loop () =
    match Atomic.get bucket_slot with
    | None ->
        let bucket_ref = make_bucket [ (key, value); ] in
        if Atomic.compare_and_set bucket_slot None (Some bucket_ref) then (
          add_size table bucket_index 1;
          None
        ) else
          loop ()
    | Some bucket_ref ->
        let rec update_bucket () =
          let bucket = Atomic.get bucket_ref in
          let (previous, next_bucket, inserted_new) = replace_in_bucket key value bucket in
          if Atomic.compare_and_set bucket_ref bucket next_bucket then (
            if inserted_new then
              add_size table bucket_index 1;
            previous
          ) else
            update_bucket ()
        in
        update_bucket ()
  in
  loop ()

let get = fun map ~key ->
  find_in_bucket key (bucket_items (bucket_slot_at map.table (hash_index map key)))

let remove = fun map ~key ->
  let table = map.table in
  let bucket_index = hash_index map key in
  let bucket_slot = bucket_slot_at table bucket_index in
  let rec loop () =
    match Atomic.get bucket_slot with
    | None -> None
    | Some bucket_ref ->
        let bucket = Atomic.get bucket_ref in
        let (removed, next_bucket, did_remove) = remove_from_bucket key bucket in
        if not did_remove then
          None
        else if Atomic.compare_and_set bucket_ref bucket next_bucket then (
          add_size table bucket_index (-1);
          removed
        ) else
          loop ()
  in
  loop ()

let has_key = fun map ~key ->
  match get map ~key with
  | Some _ -> true
  | None -> false

let length = fun map ->
  let total =
    Array.fold_left map.table.sizes ~acc:0 ~fn:(fun total size -> total + Atomic.get size)
  in
  Int.max 0 total

let is_empty = fun map -> Int.equal (length map) 0

let clear = fun map ->
  let table = map.table in
  for index = 0 to Array.length table.buckets - 1 do
    match Atomic.get (bucket_slot_at table index) with
    | None -> ()
    | Some bucket_ref ->
        let removed = Atomic.exchange bucket_ref [] in
        add_size table index (-(List.length removed))
  done

let fold_left = fun map ~init ~fn ->
  Array.fold_left
    map.table.buckets
    ~acc:init
    ~fn:(fun acc bucket_slot ->
      List.fold_left
        (bucket_items bucket_slot)
        ~acc:acc
        ~fn:(fun acc (key, value) ->
          fn acc key value))

let keys = fun map -> fold_left map ~init:[] ~fn:(fun acc key _value -> key :: acc)

let values = fun map -> fold_left map ~init:[] ~fn:(fun acc _key value -> value :: acc)

let for_each = fun map ~fn ->
  fold_left
    map
    ~init:()
    ~fn:(fun () key value ->
      fn key value;
      ())

let to_list = fun map -> fold_left map ~init:[] ~fn:(fun acc key value -> (key, value) :: acc)

let entry = fun map ~key ->
  match get map ~key with
  | Some value -> Occupied value
  | None -> Vacant

let compute = fun map ~key ~fn ->
  let table = map.table in
  let bucket_index = hash_index map key in
  let bucket_slot = bucket_slot_at table bucket_index in
  let rec loop () =
    match Atomic.get bucket_slot with
    | None -> (
        match fn None with
        | Abort result
        | Remove result -> result
        | Insert (value, result) ->
            let bucket_ref = make_bucket [ (key, value); ] in
            if Atomic.compare_and_set bucket_slot None (Some bucket_ref) then (
              add_size table bucket_index 1;
              result
            ) else
              loop ()
      )
    | Some bucket_ref -> (
        let bucket = Atomic.get bucket_ref in
        let current = find_in_bucket key bucket in
        match fn current with
        | Abort result -> result
        | Insert (value, result) ->
            let (_, next_bucket, inserted_new) = replace_in_bucket key value bucket in
            if Atomic.compare_and_set bucket_ref bucket next_bucket then (
              if inserted_new then
                add_size table bucket_index 1;
              result
            ) else
              loop ()
        | Remove result -> (
            match current with
            | None -> result
            | Some _ ->
                let (_, next_bucket, did_remove) = remove_from_bucket key bucket in
                if not did_remove then
                  result
                else if Atomic.compare_and_set bucket_ref bucket next_bucket then (
                  add_size table bucket_index (-1);
                  result
                ) else
                  loop ()
          )
      )
  in
  loop ()

let from_list = fun pairs ->
  let map = with_capacity ~size:(List.length pairs) in
  List.for_each
    pairs
    ~fn:(fun (key, value) ->
      let _ = insert map ~key ~value in
      ());
  map

let iter: type key value. (key, value) t -> (key * value) Iter.Iterator.t = fun map ->
  let module MapIter = struct
    type state = (key * value) list

    type item = key * value

    let next = fun state ->
      match state with
      | [] -> (None, [])
      | item :: rest -> (Some item, rest)

    let size = fun state -> List.length state
  end in
  Iter.Iterator.make (module MapIter) (to_list map)

let mut_iter: type key value. (key, value) t -> (key * value) Iter.MutIterator.t = fun map ->
  let module MapIter = struct
    type state = {
      mutable items: (key * value) list;
    }

    type item = key * value

    let next = fun state ->
      match state.items with
      | [] -> None
      | item :: rest ->
          state.items <- rest;
          Some item

    let size = fun state -> List.length state.items

    let clone = fun state -> { items = state.items }
  end in
  Iter.MutIterator.make (module MapIter) { items = to_list map }
