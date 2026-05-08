open Kernel

external caml_hash: int -> int -> int -> 'value -> int = "caml_hash" [@@ noalloc]

type ('key, 'value) t = {
  mutable buckets: ('key * 'value) list array;
  mutable size: int;
}

type ('key, 'value) entry =
  | Occupied of 'value
  | Vacant

type ('value, 'result) operation =
  | Insert of 'value * 'result
  | Remove of 'result
  | Abort of 'result

let minimum_capacity = 16

let normalize_capacity = fun capacity -> Int.max minimum_capacity capacity

let create = fun () -> { buckets = Array.make ~count:minimum_capacity ~value:[]; size = 0 }

let with_capacity = fun ~size ->
  let capacity = normalize_capacity size in
  { buckets = Array.make ~count:capacity ~value:[]; size = 0 }

let bucket_count = fun map -> Array.length map.buckets

let length = fun map -> map.size

let is_empty = fun map -> map.size = 0

let hash_index = fun map key ->
  let hash = caml_hash 10 100 0 key in
  (hash lsr 1) mod Array.length map.buckets

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

let needs_resize = fun map -> (map.size + 1) * 4 > Array.length map.buckets * 3

let insert_without_resize = fun map ~key ~value ->
  let index = hash_index map key in
  let (previous, bucket, inserted_new) =
    replace_in_bucket
      key
      value
      (Array.get_unchecked map.buckets ~at:index)
  in
  Array.set_unchecked map.buckets ~at:index ~value:bucket;
  if inserted_new then
    map.size <- map.size + 1;
  previous

let resize = fun map new_capacity ->
  let old_buckets = map.buckets in
  map.buckets <- Array.make ~count:new_capacity ~value:[];
  map.size <- 0;
  Array.for_each
    old_buckets
    ~fn:(fun bucket ->
      List.for_each
        bucket
        ~fn:(fun (key, value) ->
          let _ = insert_without_resize map ~key ~value in
          ()))

let insert = fun map ~key ~value ->
  if needs_resize map then
    resize map (Array.length map.buckets * 2);
  insert_without_resize map ~key ~value

let get = fun map ~key ->
  let bucket = Array.get_unchecked map.buckets ~at:(hash_index map key) in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (existing_key, value) :: rest ->
        if existing_key = key then
          Some value
        else
          loop rest
  in
  loop bucket

let remove = fun map ~key ->
  let index = hash_index map key in
  let (removed, bucket, did_remove) =
    remove_from_bucket key (Array.get_unchecked map.buckets ~at:index)
  in
  Array.set_unchecked map.buckets ~at:index ~value:bucket;
  if did_remove then
    map.size <- map.size - 1;
  removed

let has_key = fun map ~key ->
  match get map ~key with
  | Some _ -> true
  | None -> false

let clear = fun map ->
  map.buckets <- Array.make ~count:(Array.length map.buckets) ~value:[];
  map.size <- 0

let fold_left = fun map ~init ~fn ->
  Array.fold_left
    map.buckets
    ~acc:init
    ~fn:(fun acc bucket ->
      List.fold_left
        bucket
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
  let current = get map ~key in
  match fn current with
  | Abort result -> result
  | Insert (value, result) ->
      let _ = insert map ~key ~value in
      result
  | Remove result ->
      let _ = remove map ~key in
      result

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
