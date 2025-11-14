(** SwissTable HashMap Implementation
    
    Based on Google's SwissTable algorithm from hashbrown (Rust).
    
    Key concepts:
    - Control bytes (tags): 1 byte per bucket storing EMPTY/DELETED/FULL state + hash fragment
    - Group scanning: Load 8 control bytes at once and scan in parallel using bit tricks
    - Triangular probing: Quadratic probing with increasing stride
    - Load factor: 87.5% (7/8 buckets) for good performance
*)

open Kernel.Global

(* Import modules for convenience *)
module Array = Kernel.Collections.Array
module List = Kernel.Collections.List
module Option = Kernel.Option

(* === Native C Hash Functions === *)

external hash_native : 'a -> int = "swisstable_hash"
external hash_h1 : int -> int -> int = "swisstable_h1"
external hash_h2 : int -> int = "swisstable_h2"

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
  let remove_lowest_bit mask =
    mask land (mask - 1)

  (* Check if mask is empty *)
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

  (* Load 8 control bytes from array starting at index *)
  let load ctrl idx =
    let b0 = I64.of_int ctrl.(idx) in
    let b1 = I64.of_int ctrl.(idx + 1) in
    let b2 = I64.of_int ctrl.(idx + 2) in
    let b3 = I64.of_int ctrl.(idx + 3) in
    let b4 = I64.of_int ctrl.(idx + 4) in
    let b5 = I64.of_int ctrl.(idx + 5) in
    let b6 = I64.of_int ctrl.(idx + 6) in
    let b7 = I64.of_int ctrl.(idx + 7) in
    I64.logor
      (I64.logor
         (I64.logor (I64.logor b0 (I64.shift_left b1 8))
            (I64.logor (I64.shift_left b2 16) (I64.shift_left b3 24)))
         (I64.logor (I64.shift_left b4 32) (I64.shift_left b5 40)))
      (I64.logor (I64.shift_left b6 48) (I64.shift_left b7 56))

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
     Uses bit-parallel algorithm from https://graphics.stanford.edu/~seander/bithacks.html *)
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
  let match_empty group =
    let deleted_marker = repeat Tag.deleted in
    (* If top two bits are both 1, it's EMPTY (0xFF) *)
    let result = I64.logand
      (I64.logand group (I64.shift_left group 1))
      deleted_marker
    in
    bitmask_to_int result

  (* Match EMPTY or DELETED tags (high bit set) *)
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
    mutable ctrl : int array;
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
  let set_ctrl table idx tag =
    table.ctrl.(idx) <- tag;
    (* Mirror the first Group.width bytes at the end for wrap-around *)
    if idx < Group.width then
      table.ctrl.((table.bucket_mask + 1) + idx) <- tag

  (* Create empty table with given capacity *)
  let create capacity =
    let buckets = capacity_to_buckets capacity in
    let bucket_mask = buckets - 1 in
    {
      buckets = Array.make buckets None;
      ctrl = Array.make (buckets + Group.width) Tag.empty;
      len = 0;
      bucket_mask;
    }

  (* Find the bucket index for a key, or None if not found *)
  let find table key =
    if table.len = 0 then None
    else
      let hash = hash_native key in
      let h2 = hash_h2 hash in
      let probe = ProbeSeq.start hash table.bucket_mask in
      let max_probes = (table.bucket_mask + 1) / Group.width + 1 in
      
      let rec search probes_done =
        (* Safety check: prevent infinite loops *)
        if probes_done >= max_probes then None
        else
          let group = Group.load table.ctrl probe.pos in
          let matches = Group.match_tag group h2 in
          
          (* Check each matching position *)
          let rec check_matches mask =
            match BitMask.lowest_set_bit_index mask with
            | None ->
                (* No match in this group, check for EMPTY *)
                let empties = Group.match_empty group in
                if not (BitMask.is_empty empties) then None  (* Found EMPTY, key doesn't exist *)
                else begin
                  (* No EMPTY, continue probing *)
                  ProbeSeq.move_next probe table.bucket_mask;
                  search (probes_done + 1)
                end
            | Some offset ->
                let idx = (probe.pos + offset) land table.bucket_mask in
                match table.buckets.(idx) with
                | Some (k, _) when k = key -> Some idx
                | _ ->
                    (* False positive or different key, keep checking *)
                    let rest = BitMask.remove_lowest_bit mask in
                    check_matches rest
          in
          check_matches matches
      in
      search 0

  (* Find an empty slot for insertion, returns (index, needs_resize) *)
  let find_insert_slot table key =
    let hash = hash_native key in
    let h2 = hash_h2 hash in
    let probe = ProbeSeq.start hash table.bucket_mask in
    let max_probes = (table.bucket_mask + 1) / Group.width + 1 in
    
    let rec search probes_done =
      (* Safety check: if we've probed too many times, table might be full *)
      if probes_done >= max_probes then
        (* This shouldn't happen if resize logic is correct, but prevent infinite loop *)
        (0, true)  (* Force a resize *)
      else
        let group = Group.load table.ctrl probe.pos in
        let empties = Group.match_empty_or_deleted group in
        
        match BitMask.lowest_set_bit_index empties with
        | Some offset ->
            let idx = (probe.pos + offset) land table.bucket_mask in
            (idx, false)
        | None ->
            ProbeSeq.move_next probe table.bucket_mask;
            search (probes_done + 1)
    in
    search 0

  (* Resize table to new capacity *)
  let resize table new_capacity =
    let new_buckets = capacity_to_buckets new_capacity in
    let new_bucket_mask = new_buckets - 1 in
    let new_table = {
      buckets = Array.make new_buckets None;
      ctrl = Array.make (new_buckets + Group.width) Tag.empty;
      len = 0;
      bucket_mask = new_bucket_mask;
    } in
    
    (* Rehash all entries *)
    Array.iteri (fun idx bucket ->
      match bucket with
      | None -> ()
      | Some (k, v) ->
          let hash = hash_native k in
          let h2 = hash_h2 hash in
          let (new_idx, _) = find_insert_slot new_table k in
          new_table.buckets.(new_idx) <- Some (k, v);
          set_ctrl new_table new_idx (Tag.full h2);
          new_table.len <- new_table.len + 1
    ) table.buckets;
    
    new_table

  (* Insert a key-value pair, returns (new_table, previous_value) *)
  let insert table key value =
    (* Check if we need to resize *)
    let capacity = bucket_mask_to_capacity table.bucket_mask in
    let needs_resize = table.len >= capacity in
    
    (* Resize if needed BEFORE looking up the key *)
    let table = if needs_resize then resize table ((table.bucket_mask + 1) * 2) else table in
    
    (* Try to find existing key *)
    match find table key with
    | Some idx ->
        let previous = table.buckets.(idx) in
        table.buckets.(idx) <- Some (key, value);
        (table, Option.map snd previous)
      | None ->
        (* Insert new entry *)
        let hash = hash_native key in
        let h2 = hash_h2 hash in
        let (idx, _) = find_insert_slot table key in
        table.buckets.(idx) <- Some (key, value);
        set_ctrl table idx (Tag.full h2);
        table.len <- table.len + 1;
        (table, None)

  (* Remove a key, returns (new_table, removed_value) *)
  let remove table key =
    match find table key with
    | None -> (table, None)
    | Some idx ->
        let previous = table.buckets.(idx) in
        table.buckets.(idx) <- None;
        set_ctrl table idx Tag.deleted;
        table.len <- table.len - 1;
        (table, Option.map snd previous)

  (* Get value for key *)
  let get table key =
    match find table key with
    | None -> None
    | Some idx -> Option.map snd table.buckets.(idx)

  (* Check if key exists *)
  let contains_key table key =
    Option.is_some (find table key)

  (* Clear all entries *)
  let clear table =
    Array.fill table.buckets 0 (Array.length table.buckets) None;
    Array.fill table.ctrl 0 (Array.length table.ctrl) Tag.empty;
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

module Cell = Kernel.Sync.Cell

type ('k, 'v) t = ('k, 'v) RawTable.t Cell.t

(* Creation *)

let create () =
  Cell.create (RawTable.create 0)

let with_capacity capacity =
  Cell.create (RawTable.create capacity)

let of_list pairs =
  let map = create () in
  List.iter (fun (k, v) -> 
    let table = Cell.get map in
    Cell.set map (fst (RawTable.insert table k v))
  ) pairs;
  map

(* Basic operations *)

let insert map key value =
  let table = Cell.get map in
  let (new_table, previous) = RawTable.insert table key value in
  Cell.set map new_table;
  previous

let get map key =
  RawTable.get (Cell.get map) key

let remove map key =
  let table = Cell.get map in
  let (new_table, previous) = RawTable.remove table key in
  Cell.set map new_table;
  previous

let contains_key map key =
  RawTable.contains_key (Cell.get map) key

let len map = 
  let {RawTable.len; _} = Cell.get map in
  len

let is_empty map = 
  let {RawTable.len; _} = Cell.get map in
  len = 0

let clear map =
  RawTable.clear (Cell.get map)

(* Iteration *)

let keys map =
  let result = Cell.create [] in
  RawTable.iter (fun k _ -> 
    Cell.set result (k :: Cell.get result)
  ) (Cell.get map);
  Cell.get result

let values map =
  let result = Cell.create [] in
  RawTable.iter (fun _ v -> 
    Cell.set result (v :: Cell.get result)
  ) (Cell.get map);
  Cell.get result

let iter f map =
  RawTable.iter f (Cell.get map)

let fold f map acc =
  RawTable.fold f (Cell.get map) acc

let to_list map =
  RawTable.to_list (Cell.get map)

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
