(**
   SwissTable HashMap Implementation

   Based on Google's SwissTable algorithm from hashbrown (Rust).

   Key concepts:
   - Control bytes (tags): 1 byte per bucket storing EMPTY/DELETED/FULL state + hash fragment
   - Group scanning: Load 8 control bytes at once and scan in parallel using bit tricks
   - Triangular probing: Quadratic probing with increasing stride
   - Load factor: 87.5% (7/8 buckets) for good performance
*)
open Std
open Std.IO
open Std.Collections

(* === Native C Hash Functions === *)

(* OCaml's polymorphic hash function - same as Hashtbl.hash uses internally *)

external caml_hash: int -> int -> int -> 'a -> int = "caml_hash" [@@ noalloc]

(* Use OCaml's polymorphic hash for structural equality
   * This ensures two records/variants/tuples with same values hash equally
   * This fixes the bug where duplicate complex keys weren't properly deduplicated
*)

let hash_native: 'a -> int = fun key -> caml_hash 10 100 0 key

external hash_h1: int -> int -> int = "swisstable_h1"

external hash_h2: int -> int = "swisstable_h2"

(* === Native C SIMD Group Functions === *)

external group_load_simd: bytes -> int -> int64 = "swisstable_group_load"

external group_match_tag_simd: bytes -> int -> int -> int = "swisstable_group_match_tag"

external group_match_empty_simd: bytes -> int -> int = "swisstable_group_match_empty"

external group_match_empty_or_deleted_simd: bytes -> int -> int =
  "swisstable_group_match_empty_or_deleted"

(* === Native C High-Level Search Functions === *)

external find_insert_slot_simd: bytes -> int -> int -> int = "swisstable_find_insert_slot"

external find_candidates_simd: bytes -> int -> int -> int -> int list = "swisstable_find_candidates"

(* === Internal Modules === *)
(** Control byte (tag) module - manages the 1-byte metadata per bucket *)

module Tag = struct
  type t = int

  (* Tag constants *)

  let empty = 0xff

  (* 255 - bucket never used *)

  let deleted = 0x80

  (* 128 - tombstone for removed entry *)

  (* Create a FULL tag from a hash (stores top 7 bits) *)

  let full = fun hash -> hash land 0x7f

  (* Check if tag represents a full bucket (top bit is 0) *)

  let is_full = fun tag -> tag land 0x80 = 0

  (* Check if tag is a special value (EMPTY or DELETED) *)

  let is_special = fun tag -> not (tag land 0x80 = 0)

  (* Check if special tag is EMPTY (vs DELETED) *)

  let special_is_empty = fun tag ->
    assert (is_special tag);
    not (tag land 0x01 = 0)
end

(** BitMask module - wraps an int representing a bitmask of matching positions *)
module BitMask = struct
  type t = int

  let lowest_set_bit_index = fun mask ->
    if mask = 0 then
      None
    else
      (* Count trailing zeros to find position *)
      let rec count_trailing_zeros n acc =
        if n land 1 = 1 then
          acc
        else
          count_trailing_zeros (n lsr 1) (acc + 1)
      in
      Some (count_trailing_zeros mask 0)

  let remove_lowest_bit = fun mask -> mask land (mask - 1)

  let is_empty = fun mask -> mask = 0
end

(** Group module - parallel scanning of 8 control bytes *)
module Group = struct
  let width = 8

  (* Process 8 bytes at a time *)

  type t = int64

  (* Use Kernel.Int64 for all operations *)

  module I64 = Kernel.Int64

  (* Helper: replicate a byte across all 8 bytes of int64 *)

  let repeat = fun byte ->
    let b = I64.of_int byte in
    let b = I64.logor b (I64.shift_left b 8) in
    let b = I64.logor b (I64.shift_left b 16) in
    I64.logor b (I64.shift_left b 32)

  let load = fun ctrl idx ->
    (* Use SIMD-optimized C function *)
    group_load_simd ctrl idx

  (* Convert bitmask (int64) to int for BitMask module
     The result from match operations has the high bit (0x80) set for matching bytes.
     We need to extract bit 7 from each byte position and pack them into an int.
  *)

  let bitmask_to_int = fun bits ->
    let extract_bit byte_pos =
      I64.to_int
        (I64.shift_right_logical
          (I64.logand bits (I64.shift_left 0x80L (byte_pos * 8)))
          (byte_pos * 8 + 7))
    in
    extract_bit 0
    lor (extract_bit 1 lsl 1)
    lor (extract_bit 2 lsl 2)
    lor (extract_bit 3 lsl 3)
    lor (extract_bit 4 lsl 4)
    lor (extract_bit 5 lsl 5)
    lor (extract_bit 6 lsl 6)
    lor (extract_bit 7 lsl 7)

  let match_tag_impl = fun ctrl idx tag ->
    (* Use SIMD-optimized C function - directly returns bitmask *)
    group_match_tag_simd ctrl idx tag

  (* Keep this version for backwards compatibility if needed *)

  let match_tag = fun group tag ->
    let tag_repeated = repeat tag in
    let cmp = I64.logxor group tag_repeated in
    let ones = repeat 0x01 in
    let deleted_marker = repeat Tag.deleted in
    (* Find bytes that match: (cmp - 0x01...) & ~cmp & 0x80... *)
    let result = I64.logand (I64.logand (I64.sub cmp ones) (I64.lognot cmp)) deleted_marker in
    bitmask_to_int result

  let match_empty_impl = fun ctrl idx ->
    (* Use SIMD-optimized C function *)
    group_match_empty_simd ctrl idx

  (* Keep this version for backwards compatibility *)

  let match_empty = fun group ->
    let deleted_marker = repeat Tag.deleted in
    (* If top two bits are both 1, it's EMPTY (0xFF) *)
    let result = I64.logand (I64.logand group (I64.shift_left group 1)) deleted_marker in
    bitmask_to_int result

  let match_empty_or_deleted_impl = fun ctrl idx ->
    (* Use SIMD-optimized C function - this is the critical fast path! *)
    group_match_empty_or_deleted_simd ctrl idx

  (* Keep this version for backwards compatibility *)

  let match_empty_or_deleted = fun group ->
    let deleted_marker = repeat Tag.deleted in
    let result = I64.logand group deleted_marker in
    bitmask_to_int result

  (* Match FULL tags (high bit clear) *)

  let match_full = fun group ->
    let deleted_marker = repeat Tag.deleted in
    let result = I64.logand (I64.lognot group) deleted_marker in
    bitmask_to_int result
end

(** ProbeSeq module - triangular/quadratic probing sequence *)
module ProbeSeq = struct
  type t = {
    mutable pos: int;
    mutable stride: int;
  }

  (* Start probing from hash position *)

  let start = fun hash bucket_mask ->
    let pos = hash land bucket_mask in
    { pos; stride = 0 }

  (* Move to next probe position (triangular sequence) *)

  let move_next = fun seq bucket_mask ->
    seq.stride <- seq.stride + Group.width;
    seq.pos <- (seq.pos + seq.stride) land bucket_mask
end

(** RawTable module - core hash table implementation *)
module RawTable = struct
  type ('k, 'v) t = {
    mutable buckets: ('k * 'v) option array;
    mutable ctrl: bytes;
    mutable len: int;
    mutable bucket_mask: int;
  }

  (* Calculate number of buckets needed for given capacity *)

  let capacity_to_buckets = fun cap ->
    if cap < 4 then
      4
    else if cap < 8 then
      8
    else
      (* For larger tables: ensure load factor of 7/8 *)
      let adjusted = (cap * 8) / 7 in
      (* Round up to next power of 2 *)
      let rec next_pow2 n p =
        if p >= n then
          p
        else
          next_pow2 n (p * 2)
      in
      next_pow2 adjusted 8

  (* Calculate maximum load for given bucket count *)

  let bucket_mask_to_capacity = fun bucket_mask ->
    if bucket_mask < 8 then
      bucket_mask
    else
      ((bucket_mask + 1) / 8) * 7

  let set_ctrl = fun table idx tag ->
    Kernel.Bytes.unsafe_set table.ctrl idx (Kernel.Char.from_int_unchecked tag);
    (* Mirror the first Group.width bytes at the end for wrap-around *)
    if idx < Group.width then
      Kernel.Bytes.unsafe_set
        table.ctrl
        ((table.bucket_mask + 1) + idx)
        (Kernel.Char.from_int_unchecked tag)

  (* Create empty table with given capacity *)

  let create = fun capacity ->
    let buckets = capacity_to_buckets capacity in
    let bucket_mask = buckets - 1 in
    let ctrl = Kernel.Bytes.create ~size:(buckets + Group.width) in
    Kernel.Bytes.fill
      ctrl
      ~offset:0
      ~len:(buckets + Group.width)
      ~char:(Kernel.Char.from_int_unchecked Tag.empty);
    {
      buckets = Array.make ~count:buckets ~value:None;
      ctrl;
      len = 0;
      bucket_mask;
    }

  (* Find the bucket index for a key with precomputed hash, or None if not found *)

  let find_with_hash = fun table key hash h2 ->
    if table.len = 0 then
      None
    else
      (* Use C function to get candidate bucket indices - entire SIMD search in C! *)
      let candidates = find_candidates_simd table.ctrl hash h2 table.bucket_mask in
      (* Check each candidate for matching key *)
      let rec check_candidates cands =
        match cands with
        | [] -> None
        | idx :: rest ->
            match Array.get_unchecked table.buckets ~at:idx with
            | Some (k, _) when k = key -> Some idx
            | _ -> check_candidates rest
      in
      check_candidates candidates

  (* Find the bucket index for a key, or None if not found *)

  let find = fun table key ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    find_with_hash table key hash h2

  (* Find an empty slot for insertion with precomputed hash, returns (index, needs_resize) *)

  let find_insert_slot_with_hash = fun table hash ->
    (* Use C function - entire search loop in C with SIMD, no FFI overhead! *)
    let idx = find_insert_slot_simd table.ctrl hash table.bucket_mask in
    if idx < 0 then
      (0, true)
    else
      (idx, false)

  (* Find an empty slot for insertion, returns (index, needs_resize) *)

  let find_insert_slot = fun table key ->
    let hash = hash_native key in
    find_insert_slot_with_hash table hash

  (* Resize table to new capacity - mutates in place *)

  let resize = fun table new_capacity ->
    let new_buckets = capacity_to_buckets new_capacity in
    let new_bucket_mask = new_buckets - 1 in
    let old_buckets = table.buckets in
    let ctrl = Kernel.Bytes.create ~size:(new_buckets + Group.width) in
    Kernel.Bytes.fill
      ctrl
      ~offset:0
      ~len:(new_buckets + Group.width)
      ~char:(Kernel.Char.from_int_unchecked Tag.empty);
    (* Replace table contents *)
    table.buckets <- Array.make ~count:new_buckets ~value:None;
    table.ctrl <- ctrl;
    table.len <- 0;
    table.bucket_mask <- new_bucket_mask;
    (* Rehash all entries *)
    for idx = 0 to Array.length old_buckets - 1 do
      match Array.get_unchecked old_buckets ~at:idx with
      | None -> ()
      | Some (k, v) ->
          let hash = hash_native k in
          let h2 = hash_h2 hash in
          let (new_idx, _) = find_insert_slot table k in
          Array.set_unchecked table.buckets ~at:new_idx ~value:(Some (k, v));
          set_ctrl table new_idx (Tag.full h2);
          table.len <- table.len + 1
    done

  (* Insert a key-value pair, returns previous_value - mutates table *)

  let insert = fun table key value ->
    (* Check if we need to resize *)
    let capacity = bucket_mask_to_capacity table.bucket_mask in
    let needs_resize = table.len >= capacity in
    (* Resize if needed BEFORE looking up the key *)
    if needs_resize then
      resize table ((table.bucket_mask + 1) * 2);
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    (* Try to find existing key *)
    match find_with_hash table key hash h2 with
    | Some idx ->
        let previous = Array.get_unchecked table.buckets ~at:idx in
        Array.set_unchecked table.buckets ~at:idx ~value:(Some (key, value));
        Option.map previous ~fn:(fun (_, value) -> value)
    | None ->
        (* Insert new entry - reuse hash *)
        let (idx, _) = find_insert_slot_with_hash table hash in
        Array.set_unchecked table.buckets ~at:idx ~value:(Some (key, value));
        set_ctrl table idx (Tag.full h2);
        table.len <- table.len + 1;
        None

  (* Remove a key, returns removed_value - mutates table *)

  let remove = fun table key ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | None -> None
    | Some idx ->
        let previous = Array.get_unchecked table.buckets ~at:idx in
        Array.set_unchecked table.buckets ~at:idx ~value:None;
        set_ctrl table idx Tag.deleted;
        table.len <- table.len - 1;
        Option.map previous ~fn:(fun (_, value) -> value)

  (* Get value for key *)

  let get = fun table key ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | None -> None
    | Some idx ->
        Option.map (Array.get_unchecked table.buckets ~at:idx) ~fn:(fun (_, value) -> value)

  (* Check if key exists *)

  let contains_key = fun table key ->
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    Option.is_some (find_with_hash table key hash h2)

  (* Clear all entries *)

  let clear = fun table ->
    for idx = 0 to Array.length table.buckets - 1 do
      Array.set_unchecked table.buckets ~at:idx ~value:None
    done;
    Kernel.Bytes.fill
      table.ctrl
      ~offset:0
      ~len:(Kernel.Bytes.length table.ctrl)
      ~char:(Kernel.Char.from_int_unchecked Tag.empty);
    table.len <- 0

  (* Iterate over all key-value pairs *)

  let iter = fun f table ->
    Array.for_each
      table.buckets
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Some (k, v) -> f k v
        | None -> ())

  (* Fold over all key-value pairs *)

  let fold = fun f table acc ->
    Array.fold_left
      table.buckets
      ~init:acc
      ~fn:(fun acc bucket ->
        match bucket with
        | Some (k, v) -> f k v acc
        | None -> acc)

  (* Convert to list *)

  let to_list = fun table ->
    Array.fold_left
      table.buckets
      ~init:[]
      ~fn:(fun acc bucket ->
        match bucket with
        | Some (k, v) -> (k, v) :: acc
        | None -> acc)
end

(* === Public API === *)

type ('k, 'v) t = ('k, 'v) RawTable.t

(* Creation *)

let create = fun () -> RawTable.create 0

let with_capacity = fun capacity -> RawTable.create capacity

let of_list = fun pairs ->
  let map = create () in
  List.for_each
    pairs
    ~fn:(fun (k, v) ->
      let _ = RawTable.insert map k v in
      ());
  map

(* Basic operations *)

let insert = fun map key value -> RawTable.insert map key value

let get = fun map key -> RawTable.get map key

let remove = fun map key -> RawTable.remove map key

let contains_key = fun map key -> RawTable.contains_key map key

let len = fun map -> map.RawTable.len

let is_empty = fun map -> map.RawTable.len = 0

let clear = fun map -> RawTable.clear map

(* Iteration *)

module Cell = Sync.Cell

let keys = fun map ->
  let result = Cell.create [] in
  RawTable.iter (fun k _ -> Cell.set result (k :: Cell.get result)) map;
  Cell.get result

let values = fun map ->
  let result = Cell.create [] in
  RawTable.iter (fun _ v -> Cell.set result (v :: Cell.get result)) map;
  Cell.get result

let iter = fun f map -> RawTable.iter f map

let fold = fun f map acc -> RawTable.fold f map acc

let to_list = fun map -> RawTable.to_list map

(* Entry API *)

type ('k, 'v) entry =
  | Occupied of 'v
  | Vacant

let entry = fun map key ->
  match get map key with
  | Some v -> Occupied v
  | None -> Vacant

let or_insert = fun map key default ->
  match get map key with
  | Some v -> v
  | None ->
      let _ = insert map key default in
      default

let and_modify = fun map key f ->
  match get map key with
  | Some v ->
      let _ = insert map key (f v) in
      ()
  | None -> ()

(* Iterators *)

let into_iter: type k v. (k, v) t -> (k * v) Iter.Iterator.t = fun map ->
  let module MapIter = struct
    type state = {
      items: (k * v) list;
      pos: int;
    }

    type item = k * v

    let next = fun state ->
      if state.pos >= List.length state.items then
        (None, state)
      else
        let item = List.get_unchecked state.items ~at:state.pos in
        (Some item, { state with pos = state.pos + 1 })

    let size = fun state -> max 0 (List.length state.items - state.pos)
  end in
  let items = to_list map in
  Iter.Iterator.make (module MapIter) { MapIter.items; pos = 0 }

let to_mut_iter: type k v. (k, v) t -> (k * v) Iter.MutIterator.t = fun map ->
  let module MapIter = struct
    type state = {
      items: (k * v) list;
      mutable pos: int;
    }

    type item = k * v

    let next = fun state ->
      if state.pos >= List.length state.items then
        None
      else
        let item = List.get_unchecked state.items ~at:state.pos in
        state.pos <- state.pos + 1;
      Some item

    let size = fun state -> max 0 (List.length state.items - state.pos)

    let clone = fun state -> { items = state.items; pos = state.pos }
  end in
  let items = to_list map in
  Iter.MutIterator.make (module MapIter) { MapIter.items; pos = 0 }
