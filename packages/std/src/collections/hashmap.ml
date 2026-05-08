open Kernel

external caml_hash: int -> int -> int -> 'value -> int = "caml_hash" [@@ noalloc]

external find_insert_slot: bytes -> int -> int -> int = "std_hashmap_find_insert_slot"

external find_candidates: bytes -> int -> int -> int -> int list = "std_hashmap_find_candidates"

type ('key, 'value) entry =
  | Occupied of 'value
  | Vacant

type ('value, 'result) operation =
  | Insert of 'value * 'result
  | Remove of 'result
  | Abort of 'result

let group_width = 8

let hash_native = fun key -> caml_hash 10 100 0 key

let hash_h2 = fun hash -> (hash lsr 20) land 0x7f

module Tag = struct
  let empty = 0xff

  let deleted = 0x80

  let full = fun hash -> hash land 0x7f
end

module RawTable = struct
  type ('key, 'value) t = {
    mutable buckets: ('key * 'value) option array;
    mutable ctrl: bytes;
    mutable length: int;
    mutable bucket_mask: int;
  }

  let capacity_to_buckets = fun capacity ->
    let capacity = Int.max 0 capacity in
    if capacity < 4 then
      4
    else if capacity < 8 then
      8
    else
      let adjusted = (capacity * 8) / 7 in
      let rec next_power_of_two = fun value power ->
        if power >= value then
          power
        else
          next_power_of_two value (power * 2)
      in
      next_power_of_two adjusted 8

  let bucket_mask_to_capacity = fun bucket_mask ->
    if bucket_mask < 8 then
      bucket_mask
    else
      ((bucket_mask + 1) / 8) * 7

  let bucket_count = fun table -> Array.length table.buckets

  let set_ctrl = fun table index tag ->
    Bytes.unsafe_set table.ctrl index (Char.from_int_unchecked tag);
    if index < group_width then
      Bytes.unsafe_set table.ctrl (bucket_count table + index) (Char.from_int_unchecked tag)

  let create = fun capacity ->
    let buckets = capacity_to_buckets capacity in
    let bucket_mask = buckets - 1 in
    let ctrl = Bytes.create ~size:(buckets + group_width) in
    Bytes.fill ctrl ~offset:0 ~len:(buckets + group_width) ~char:(Char.from_int_unchecked Tag.empty);
    {
      buckets = Array.make ~count:buckets ~value:None;
      ctrl;
      length = 0;
      bucket_mask;
    }

  let find_with_hash = fun table key hash h2 ->
    if table.length = 0 then
      None
    else
      let candidates = find_candidates table.ctrl hash h2 table.bucket_mask in
      let rec loop = fun candidates ->
        match candidates with
        | [] -> None
        | index :: rest -> (
            match Array.get_unchecked table.buckets ~at:index with
            | Some (stored_key, _) when stored_key = key -> Some index
            | _ -> loop rest
          )
      in
      loop candidates

  let find = fun table key ->
    let hash = hash_native key in
    find_with_hash table key hash (hash_h2 hash)

  let rec insert_into_empty_slot = fun table key value hash h2 ->
    let index = find_insert_slot table.ctrl hash table.bucket_mask in
    if index < 0 then (
      resize table (bucket_count table * 2);
      insert_into_empty_slot table key value hash h2
    ) else (
      Array.set_unchecked table.buckets ~at:index ~value:(Some (key, value));
      set_ctrl table index (Tag.full h2);
      table.length <- table.length + 1;
      None
    )

  and resize = fun table new_capacity ->
    let new_bucket_count = capacity_to_buckets new_capacity in
    let old_buckets = table.buckets in
    table.buckets <- Array.make ~count:new_bucket_count ~value:None;
    table.ctrl <- Bytes.create ~size:(new_bucket_count + group_width);
    Bytes.fill
      table.ctrl
      ~offset:0
      ~len:(new_bucket_count + group_width)
      ~char:(Char.from_int_unchecked Tag.empty);
    table.length <- 0;
    table.bucket_mask <- new_bucket_count - 1;
    Array.for_each
      old_buckets
      ~fn:(fun bucket ->
        match bucket with
        | None -> ()
        | Some (key, value) ->
            let hash = hash_native key in
            let h2 = hash_h2 hash in
            let _ = insert_into_empty_slot table key value hash h2 in
            ())

  let insert = fun table key value ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | Some index ->
        let previous = Array.get_unchecked table.buckets ~at:index in
        Array.set_unchecked table.buckets ~at:index ~value:(Some (key, value));
        Option.map previous ~fn:(fun (_, value) -> value)
    | None ->
        if table.length >= bucket_mask_to_capacity table.bucket_mask then
          resize table (bucket_count table * 2);
        insert_into_empty_slot table key value hash h2

  let get = fun table key ->
    match find table key with
    | None -> None
    | Some index ->
        Option.map (Array.get_unchecked table.buckets ~at:index) ~fn:(fun (_, value) -> value)

  let remove = fun table key ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | None -> None
    | Some index ->
        let previous = Array.get_unchecked table.buckets ~at:index in
        Array.set_unchecked table.buckets ~at:index ~value:None;
        set_ctrl table index Tag.deleted;
        table.length <- table.length - 1;
        Option.map previous ~fn:(fun (_, value) -> value)

  let has_key = fun table key ->
    let hash = hash_native key in
    Option.is_some (find_with_hash table key hash (hash_h2 hash))

  let clear = fun table ->
    for index = 0 to bucket_count table - 1 do
      Array.set_unchecked table.buckets ~at:index ~value:None
    done;
    Bytes.fill
      table.ctrl
      ~offset:0
      ~len:(Bytes.length table.ctrl)
      ~char:(Char.from_int_unchecked Tag.empty);
    table.length <- 0

  let for_each = fun table ~fn ->
    Array.for_each
      table.buckets
      ~fn:(fun bucket ->
        match bucket with
        | Some (key, value) -> fn key value
        | None -> ())

  let fold_left = fun table ~init ~fn ->
    Array.fold_left
      table.buckets
      ~acc:init
      ~fn:(fun acc bucket ->
        match bucket with
        | Some (key, value) -> fn acc key value
        | None -> acc)

  let to_list = fun table -> fold_left table ~init:[] ~fn:(fun acc key value -> (key, value) :: acc)
end

type ('key, 'value) t = ('key, 'value) RawTable.t

let create = fun () -> RawTable.create 0

let with_capacity = fun ~size -> RawTable.create size

let from_list = fun pairs ->
  let map = with_capacity ~size:(List.length pairs) in
  List.for_each
    pairs
    ~fn:(fun (key, value) ->
      let _ = RawTable.insert map key value in
      ());
  map

let bucket_count = RawTable.bucket_count

let insert = fun map ~key ~value -> RawTable.insert map key value

let get = fun map ~key -> RawTable.get map key

let remove = fun map ~key -> RawTable.remove map key

let has_key = fun map ~key -> RawTable.has_key map key

let length = fun map -> map.RawTable.length

let is_empty = fun map -> Int.equal map.RawTable.length 0

let clear = RawTable.clear

let keys = fun map -> RawTable.fold_left map ~init:[] ~fn:(fun acc key _value -> key :: acc)

let values = fun map -> RawTable.fold_left map ~init:[] ~fn:(fun acc _key value -> value :: acc)

let for_each = RawTable.for_each

let fold_left = RawTable.fold_left

let to_list = RawTable.to_list

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
