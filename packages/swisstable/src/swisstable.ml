(** SwissTable HashMap Implementation
    
    Based on Google's SwissTable algorithm from hashbrown (Rust).
    
    Key concepts:
    - Control bytes (tags): 1 byte per bucket storing EMPTY/DELETED/FULL state + hash fragment
    - Group scanning: Load 8 control bytes at once and scan in parallel using bit tricks
    - Triangular probing: Quadratic probing with increasing stride
    - Load factor: 87.5% (7/8 buckets) for good performance
*)

open Kernel.Global
open Std.IO

(* Import modules for convenience *)
module Array = Kernel.Collections.Array
module List = Kernel.Collections.List
module Option = Kernel.Option

(* === Native C Hash Functions === *)

external hash_native : 'a -> int = "swisstable_hash"
external hash_h1 : int -> int -> int = "swisstable_h1"
external hash_h2 : int -> int = "swisstable_h2"

(* === Native C SIMD Group Functions === *)

external group_load_simd : bytes -> int -> int64 = "swisstable_group_load"
external group_match_tag_simd : bytes -> int -> int -> int = "swisstable_group_match_tag"
external group_match_empty_simd : bytes -> int -> int = "swisstable_group_match_empty"
external group_match_empty_or_deleted_simd : bytes -> int -> int = "swisstable_group_match_empty_or_deleted"

(* === Native C High-Level Search Functions === *)

external find_insert_slot_simd : bytes -> int -> int -> int = "swisstable_find_insert_slot"
external find_candidates_simd : bytes -> int -> int -> int -> int list = "swisstable_find_candidates"

(* === Internal Modules === *)

(** Control byte (tag) module - manages the 1-byte metadata per bucket *)
module Tag = struct
  type t = int

  (* Tag constants *)
  let empty = 0xFF    (* 255 - bucket never used *)
  let deleted = 0x80  (* 128 - tombstone for removed entry *)

  (* Create a FULL tag from a hash (stores top 7 bits) *)
  let full hash = hash land 0x7F

  (* Check if tag represents a full bucket (top bit is 0) *)
  let is_full tag = tag land 0x80 = 0

  (* Check if tag is a special value (EMPTY or DELETED) *)
  let is_special tag = not (tag land 0x80 = 0)

  (* Check if special tag is EMPTY (vs DELETED) *)
  let special_is_empty tag =
    assert (is_special tag);
    not (tag land 0x01 = 0)
end

(** BitMask module - wraps an int representing a bitmask of matching positions *)
module BitMask = struct
  type t = int

  (* Find the index of the lowest set bit *)
  [@inline always]
  let lowest_set_bit_index mask =
    if mask = 0 then None
    else
      (* Count trailing zeros to find position *)
      let rec count_trailing_zeros n acc =
        if n land 1 = 1 then acc
        else count_trailing_zeros (n lsr 1) (acc + 1)
      in
      Some (count_trailing_zeros mask 0)

  (* Remove the lowest set bit from mask *)
  [@inline always]
  let remove_lowest_bit mask =
    mask land (mask - 1)

  (* Check if mask is empty *)
  [@inline always]
  let is_empty mask = mask = 0
end

(** Group module - parallel scanning of 8 control bytes *)
module Group = struct
  let width = 8  (* Process 8 bytes at a time *)

  type t = int64
  
  (* Use Kernel.Int64 for all operations *)
  module I64 = Kernel.Int64

  (* Helper: replicate a byte across all 8 bytes of int64 *)
  let repeat byte =
    let b = I64.of_int byte in
    let b = I64.logor b (I64.shift_left b 8) in
    let b = I64.logor b (I64.shift_left b 16) in
    I64.logor b (I64.shift_left b 32)

  (* Load 8 control bytes from bytes starting at index *)
  [@inline always]
  let load ctrl idx =
    (* Use SIMD-optimized C function *)
    group_load_simd ctrl idx

  (* Convert bitmask (int64) to int for BitMask module 
     The result from match operations has the high bit (0x80) set for matching bytes.
     We need to extract bit 7 from each byte position and pack them into an int. *)
  let bitmask_to_int bits =
    let extract_bit byte_pos =
      I64.to_int (I64.shift_right_logical (I64.logand bits (I64.shift_left 0x80L (byte_pos * 8))) (byte_pos * 8 + 7))
    in
    extract_bit 0 lor
    (extract_bit 1 lsl 1) lor
    (extract_bit 2 lsl 2) lor
    (extract_bit 3 lsl 3) lor
    (extract_bit 4 lsl 4) lor
    (extract_bit 5 lsl 5) lor
    (extract_bit 6 lsl 6) lor
    (extract_bit 7 lsl 7)

  (* Match a specific tag in the group - returns bitmask of matches
     Uses SIMD-optimized C function for parallel byte comparison *)
  [@inline always]
  let match_tag_impl ctrl idx tag =
    (* Use SIMD-optimized C function - directly returns bitmask *)
    group_match_tag_simd ctrl idx tag
  
  (* Keep this version for backwards compatibility if needed *)
  let match_tag group tag =
    let tag_repeated = repeat tag in
    let cmp = I64.logxor group tag_repeated in
    let ones = repeat 0x01 in
    let deleted_marker = repeat Tag.deleted in
    (* Find bytes that match: (cmp - 0x01...) & ~cmp & 0x80... *)
    let result = I64.logand
      (I64.logand (I64.sub cmp ones) (I64.lognot cmp))
      deleted_marker
    in
    bitmask_to_int result

  (* Match EMPTY tags (0xFF) in the group *)
  [@inline always]
  let match_empty_impl ctrl idx =
    (* Use SIMD-optimized C function *)
    group_match_empty_simd ctrl idx
  
  (* Keep this version for backwards compatibility *)
  let match_empty group =
    let deleted_marker = repeat Tag.deleted in
    (* If top two bits are both 1, it's EMPTY (0xFF) *)
    let result = I64.logand
      (I64.logand group (I64.shift_left group 1))
      deleted_marker
    in
    bitmask_to_int result

  (* Match EMPTY or DELETED tags (high bit set) *)
  [@inline always]
  let match_empty_or_deleted_impl ctrl idx =
    (* Use SIMD-optimized C function - this is the critical fast path! *)
    group_match_empty_or_deleted_simd ctrl idx
  
  (* Keep this version for backwards compatibility *)
  let match_empty_or_deleted group =
    let deleted_marker = repeat Tag.deleted in
    let result = I64.logand group deleted_marker in
    bitmask_to_int result

  (* Match FULL tags (high bit clear) *)
  let match_full group =
    let deleted_marker = repeat Tag.deleted in
    let result = I64.logand (I64.lognot group) deleted_marker in
    bitmask_to_int result
end

(** ProbeSeq module - triangular/quadratic probing sequence *)
module ProbeSeq = struct
  type t = {
    mutable pos : int;
    mutable stride : int;
  }

  (* Start probing from hash position *)
  let start hash bucket_mask =
    let pos = hash land bucket_mask in
    { pos; stride = 0 }

  (* Move to next probe position (triangular sequence) *)
  let move_next seq bucket_mask =
    seq.stride <- seq.stride + Group.width;
    seq.pos <- (seq.pos + seq.stride) land bucket_mask
end

(** RawTable module - core hash table implementation *)
module RawTable = struct
  type ('k, 'v) t = {
    mutable buckets : ('k * 'v) option array;
    mutable ctrl : bytes;
    mutable len : int;
    mutable bucket_mask : int;
  }

  (* Calculate number of buckets needed for given capacity *)
  let capacity_to_buckets cap =
    if cap < 4 then 4
    else if cap < 8 then 8
    else
      (* For larger tables: ensure load factor of 7/8 *)
      let adjusted = (cap * 8) / 7 in
      (* Round up to next power of 2 *)
      let rec next_pow2 n p =
        if p >= n then p
        else next_pow2 n (p * 2)
      in
      next_pow2 adjusted 8

  (* Calculate maximum load for given bucket count *)
  let bucket_mask_to_capacity bucket_mask =
    if bucket_mask < 8 then bucket_mask
    else ((bucket_mask + 1) / 8) * 7

  (* Helper: Set a control byte and update the mirror if needed *)
  [@inline always]
  let set_ctrl table idx tag =
    Bytes.unsafe_set table.ctrl idx (Kernel.Char.chr tag);
    (* Mirror the first Group.width bytes at the end for wrap-around *)
    if idx < Group.width then
      Bytes.unsafe_set table.ctrl ((table.bucket_mask + 1) + idx) (Kernel.Char.chr tag)

  (* Create empty table with given capacity *)
  let create capacity =
    let buckets = capacity_to_buckets capacity in
    let bucket_mask = buckets - 1 in
    {
      buckets = Array.make buckets None;
      ctrl = Bytes.make (buckets + Group.width) (Kernel.Char.chr Tag.empty);
      len = 0;
      bucket_mask;
    }

  (* Find the bucket index for a key with precomputed hash, or None if not found *)
  let find_with_hash table key hash h2 =
    if table.len = 0 then None
    else
      (* Use C function to get candidate bucket indices - entire SIMD search in C! *)
      let candidates = find_candidates_simd table.ctrl hash h2 table.bucket_mask in
      
      (* Check each candidate for matching key *)
      let rec check_candidates cands =
        match cands with
        | [] -> None
        | idx :: rest ->
            match table.buckets.(idx) with
            | Some (k, _) when k = key -> Some idx
            | _ -> check_candidates rest
      in
      check_candidates candidates

  (* Find the bucket index for a key, or None if not found *)
  let find table key =
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    find_with_hash table key hash h2

  (* Find an empty slot for insertion with precomputed hash, returns (index, needs_resize) *)
  let find_insert_slot_with_hash table hash =
    (* Use C function - entire search loop in C with SIMD, no FFI overhead! *)
    let idx = find_insert_slot_simd table.ctrl hash table.bucket_mask in
    if idx < 0 then
      (* Table full - force resize *)
      (0, true)
    else
      (idx, false)

  (* Find an empty slot for insertion, returns (index, needs_resize) *)
  let find_insert_slot table key =
    let hash = hash_native key in
    find_insert_slot_with_hash table hash

  (* Resize table to new capacity - mutates in place *)
  let resize table new_capacity =
    let new_buckets = capacity_to_buckets new_capacity in
    let new_bucket_mask = new_buckets - 1 in
    let old_buckets = table.buckets in
    
    (* Replace table contents *)
    table.buckets <- Array.make new_buckets None;
    table.ctrl <- Bytes.make (new_buckets + Group.width) (Kernel.Char.chr Tag.empty);
    table.len <- 0;
    table.bucket_mask <- new_bucket_mask;
    
    (* Rehash all entries *)
    Array.iteri (fun idx bucket ->
      match bucket with
      | None -> ()
      | Some (k, v) ->
          let hash = hash_native k in
          let h2 = hash_h2 hash in
          let (new_idx, _) = find_insert_slot table k in
          table.buckets.(new_idx) <- Some (k, v);
          set_ctrl table new_idx (Tag.full h2);
          table.len <- table.len + 1
    ) old_buckets

  (* Insert a key-value pair, returns previous_value - mutates table *)
  let insert table key value =
    (* Check if we need to resize *)
    let capacity = bucket_mask_to_capacity table.bucket_mask in
    let needs_resize = table.len >= capacity in
    
    (* Resize if needed BEFORE looking up the key *)
    if needs_resize then resize table ((table.bucket_mask + 1) * 2);
    
    (* Compute hash once and reuse *)
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    
    (* Try to find existing key *)
    match find_with_hash table key hash h2 with
    | Some idx ->
        let previous = table.buckets.(idx) in
        table.buckets.(idx) <- Some (key, value);
        Option.map snd previous
      | None ->
        (* Insert new entry - reuse hash *)
        let (idx, _) = find_insert_slot_with_hash table hash in
        table.buckets.(idx) <- Some (key, value);
        set_ctrl table idx (Tag.full h2);
        table.len <- table.len + 1;
        None

  (* Remove a key, returns removed_value - mutates table *)
  let remove table key =
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | None -> None
    | Some idx ->
        let previous = table.buckets.(idx) in
        table.buckets.(idx) <- None;
        set_ctrl table idx Tag.deleted;
        table.len <- table.len - 1;
        Option.map snd previous

  (* Get value for key *)
  let get table key =
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    match find_with_hash table key hash h2 with
    | None -> None
    | Some idx -> Option.map snd table.buckets.(idx)

  (* Check if key exists *)
  let contains_key table key =
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    Option.is_some (find_with_hash table key hash h2)

  (* Clear all entries *)
  let clear table =
    Array.fill table.buckets 0 (Array.length table.buckets) None;
    Bytes.fill table.ctrl 0 (Bytes.length table.ctrl) (Kernel.Char.chr Tag.empty);
    table.len <- 0

  (* Iterate over all key-value pairs *)
  let iter f table =
    Array.iter (function
      | Some (k, v) -> f k v
      | None -> ()
    ) table.buckets

  (* Fold over all key-value pairs *)
  let fold f table acc =
    Array.fold_left (fun acc bucket ->
      match bucket with
      | Some (k, v) -> f k v acc
      | None -> acc
    ) acc table.buckets

  (* Convert to list *)
  let to_list table =
    Array.fold_left (fun acc bucket ->
      match bucket with
      | Some (k, v) -> (k, v) :: acc
      | None -> acc
    ) [] table.buckets
end

(* === Public API === *)

type ('k, 'v) t = ('k, 'v) RawTable.t

(* Creation *)

let create () =
  RawTable.create 0

let with_capacity capacity =
  RawTable.create capacity

let of_list pairs =
  let map = create () in
  List.iter (fun (k, v) -> 
    let _ = RawTable.insert map k v in
    ()
  ) pairs;
  map

(* Basic operations *)

let insert map key value =
  RawTable.insert map key value

let get map key =
  RawTable.get map key

let remove map key =
  RawTable.remove map key

let contains_key map key =
  RawTable.contains_key map key

let len map = 
  map.RawTable.len

let is_empty map = 
  map.RawTable.len = 0

let clear map =
  RawTable.clear map

(* Iteration *)

module Cell = Kernel.Sync.Cell

let keys map =
  let result = Cell.create [] in
  RawTable.iter (fun k _ -> 
    Cell.set result (k :: Cell.get result)
  ) map;
  Cell.get result

let values map =
  let result = Cell.create [] in
  RawTable.iter (fun _ v -> 
    Cell.set result (v :: Cell.get result)
  ) map;
  Cell.get result

let iter f map =
  RawTable.iter f map

let fold f map acc =
  RawTable.fold f map acc

let to_list map =
  RawTable.to_list map

(* Entry API *)

type ('k, 'v) entry =
  | Occupied of 'v
  | Vacant

let entry map key =
  match get map key with
  | Some v -> Occupied v
  | None -> Vacant

let or_insert map key default =
  match get map key with
  | Some v -> v
  | None ->
      let _ = insert map key default in
      default

let and_modify map key f =
  match get map key with
  | Some v -> let _ = insert map key (f v) in ()
  | None -> ()

(* Iterators *)

let into_iter : type k v. (k, v) t -> (k * v) Kernel.Iter.Iterator.t =
 fun map ->
  let module MapIter = struct
    type state = { items : (k * v) list; pos : int }
    type item = k * v

    let next state =
      if state.pos >= List.length state.items then (None, state)
      else
        let item = List.nth state.items state.pos in
        (Some item, { state with pos = state.pos + 1 })

    let size state = max 0 (List.length state.items - state.pos)
  end in
  let items = to_list map in
  Kernel.Iter.Iterator.make (module MapIter) { MapIter.items; pos = 0 }

let to_mut_iter : type k v. (k, v) t -> (k * v) Kernel.Iter.MutIterator.t =
 fun map ->
  let module MapIter = struct
    type state = { items : (k * v) list; mutable pos : int }
    type item = k * v

    let next state =
      if state.pos >= List.length state.items then None
      else
        let item = List.nth state.items state.pos in
        state.pos <- state.pos + 1;
        Some item

    let size state = max 0 (List.length state.items - state.pos)
    let clone state = { items = state.items; pos = state.pos }
  end in
  let items = to_list map in
  Kernel.Iter.MutIterator.make (module MapIter) { MapIter.items; pos = 0 }
