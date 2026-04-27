open Kernel

external caml_hash: int -> int -> int -> 'value -> int = "caml_hash" [@@ noalloc]

type ('key, 'value) t = {
  mutable buckets: ('key * 'value) list array;
  mutable size: int;
}

let minimum_capacity = 16

let normalize_capacity = fun capacity -> Int.max minimum_capacity capacity

let create = fun () -> { buckets = Array.make ~count:minimum_capacity ~value:[]; size = 0 }

let with_capacity = fun ~size ->
  let capacity = normalize_capacity size in
  { buckets = Array.make ~count:capacity ~value:[]; size = 0 }

let length = fun map -> map.size

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
  let rec loop = function
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

let for_each = fun map ~fn ->
  Array.for_each
    map.buckets
    ~fn:(fun bucket -> List.for_each bucket ~fn:(fun (key, value) -> fn key value))
