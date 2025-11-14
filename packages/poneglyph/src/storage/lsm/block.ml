(** Block - Fixed-size sorted data blocks for LSM storage *)

open Std
open Std.Collections
open Std.Sync

module Bytes = Kernel.IO.Bytes

(** Block constants *)
let max_block_size = 16384  (* 16KB *)
let header_size = 128
let max_data_size = max_block_size - header_size

(** Magic number for block format *)
let magic = "BLOK"
let version = 1

(** Entry in the block: offset and size information *)
type entry = {
  key_offset : int;
  value_offset : int;
  value_size : int;
}

(** Block structure
    
    We store:
    - entries: metadata about where each key-value pair is
    - data: packed bytes containing all keys and values
    - last_key_opt: cached last key for sort checking
*)
type t = {
  entries : entry Vector.t;
  data : bytes Cell.t;  (* mutable cell for efficient appending *)
  mutable data_pos : int;  (* current write position in data *)
}

let create () =
  {
    entries = Vector.create ();
    data = cell (Bytes.create max_data_size);
    data_pos = 0;
  }

let count t = Vector.len t.entries

let is_empty t = count t = 0

let size t =
  (* Header + entry table + actual data *)
  header_size + (count t * 12) + t.data_pos

(** Get key bytes from block data *)
let get_key t entry =
  let data = Cell.get t.data in
  Bytes.sub data entry.key_offset 41  (* Keys are always 41 bytes *)

(** Get value bytes from block data *)
let get_value t entry =
  let data = Cell.get t.data in
  Bytes.sub data entry.value_offset entry.value_size

let first_key t =
  if is_empty t then None
  else
    let entry = Vector.get t.entries 0 |> Option.expect ~msg:"first entry exists" in
    Some (get_key t entry)

let last_key t =
  let len = count t in
  if len = 0 then None
  else
    let entry = Vector.get t.entries (len - 1) |> Option.expect ~msg:"last entry exists" in
    Some (get_key t entry)

let add t ~key ~value =
  (* Validate key size *)
  if Bytes.length key != 41 then
    Error ("Key must be exactly 41 bytes, got " ^ string_of_int (Bytes.length key))
  else
    (* Check if key is greater than last key (must be sorted) *)
    match last_key t with
    | Some last when Bytes.compare key last <= 0 ->
        Error "Keys must be added in strictly increasing order"
    | _ ->
        let value_len = Bytes.length value in
        let needed_space = 41 + value_len in
        
        (* Check if we have space *)
        if t.data_pos + needed_space > max_data_size then
          Error ("Block full: cannot fit " ^ string_of_int needed_space ^ " more bytes")
        else (
          (* Write key and value to data buffer *)
          let data = Cell.get t.data in
          let key_offset = t.data_pos in
          Bytes.blit key 0 data key_offset 41;
          
          let value_offset = t.data_pos + 41 in
          Bytes.blit value 0 data value_offset value_len;
          
          (* Update position *)
          t.data_pos <- t.data_pos + needed_space;
          
          (* Add entry *)
          Vector.push t.entries { key_offset; value_offset; value_size = value_len };
          
          Ok t
        )

let get t ~key =
  if Bytes.length key != 41 then None
  else
    (* Binary search over entries *)
    let rec search low high =
      if low > high then None
      else
        let mid = low + (high - low) / 2 in
        let entry = Vector.get t.entries mid |> Option.expect ~msg:"mid in range" in
        let entry_key = get_key t entry in
        
        match Bytes.compare key entry_key with
        | 0 -> Some (get_value t entry)
        | n when n < 0 -> search low (mid - 1)
        | _ -> search (mid + 1) high
    in
    search 0 (count t - 1)

let iter t ~f =
  Vector.iter (fun entry ->
    let key = get_key t entry in
    let value = get_value t entry in
    f ~key ~value
  ) t.entries

let fold t ~init ~f =
  let acc = cell init in
  Vector.iter (fun entry ->
    let key = get_key t entry in
    let value = get_value t entry in
    let new_acc = f ~acc:(Cell.get acc) ~key ~value in
    Cell.set acc new_acc
  ) t.entries;
  Cell.get acc

(** Compute xxHash64 checksum of data
    
    For now we use a simple checksum (XOR of all 8-byte chunks).
    In production, we'd use a real xxHash implementation.
*)
let compute_checksum data len =
  let rec loop pos acc =
    if pos >= len then acc
    else if pos + 8 <= len then
      let chunk = Bytes.get_int64_be data pos in
      loop (pos + 8) (Int64.logxor acc chunk)
    else
      (* Handle remaining bytes *)
      let rec handle_tail p a =
        if p >= len then a
        else
          let byte = Int64.of_int (Bytes.get_uint8 data p) in
          handle_tail (p + 1) (Int64.logxor a byte)
      in
      handle_tail pos acc
  in
  loop 0 0L

let to_bytes t =
  let total_size = size t in
  let buf = Bytes.create total_size in
  
  (* Write header *)
  Bytes.blit_string magic 0 buf 0 4;
  Bytes.set_uint8 buf 4 version;
  Bytes.set_uint16_be buf 5 (count t);
  
  (* Write first and last keys (or zeros if empty) *)
  (match first_key t with
   | Some k -> Bytes.blit k 0 buf 7 41
   | None -> ());
  
  (match last_key t with
   | Some k -> Bytes.blit k 0 buf 48 41
   | None -> ());
  
  (* Write data size *)
  Bytes.set_int32_be buf 89 (Int32.of_int t.data_pos);
  
  (* Compute and write checksum *)
  let data = Cell.get t.data in
  let checksum = compute_checksum data t.data_pos in
  Bytes.set_int64_be buf 93 checksum;
  
  (* Reserved bytes (101-127) are already zero *)
  
  (* Write entry table *)
  let entry_start = header_size in
  for i = 0 to count t - 1 do
    let entry = Vector.get t.entries i |> Option.expect ~msg:"entry in range" in
    let offset = entry_start + (i * 12) in
    Bytes.set_int32_be buf offset (Int32.of_int entry.key_offset);
    Bytes.set_int32_be buf (offset + 4) (Int32.of_int entry.value_offset);
    Bytes.set_int32_be buf (offset + 8) (Int32.of_int entry.value_size);
  done;
  
  (* Write data *)
  let data_start = entry_start + (count t * 12) in
  Bytes.blit data 0 buf data_start t.data_pos;
  
  buf

let from_bytes buf =
  let len = Bytes.length buf in
  
  (* Validate minimum size *)
  if len < header_size then
    Error ("Block too small: " ^ string_of_int len ^ " bytes")
  else
    (* Read and validate magic *)
    let magic_bytes = Bytes.sub buf 0 4 in
    if Bytes.to_string magic_bytes != magic then
      Error "Invalid block magic number"
    else
      (* Read version *)
      let ver = Bytes.get_uint8 buf 4 in
      if ver != version then
        Error ("Unsupported block version: " ^ string_of_int ver)
      else
        (* Read entry count *)
        let entry_count = Bytes.get_uint16_be buf 5 in
        
        (* Read data size *)
        let data_size = Int32.to_int (Bytes.get_int32_be buf 89) in
        
        (* Read checksum *)
        let stored_checksum = Bytes.get_int64_be buf 93 in
        
        (* Calculate expected total size *)
        let entry_table_size = entry_count * 12 in
        let expected_size = header_size + entry_table_size + data_size in
        
        if len != expected_size then
          Error ("Block size mismatch: expected " ^ string_of_int expected_size 
                ^ " got " ^ string_of_int len)
        else
          (* Extract data and verify checksum *)
          let data_start = header_size + entry_table_size in
          let data = Bytes.sub buf data_start data_size in
          let computed_checksum = compute_checksum data data_size in
          
          if computed_checksum != stored_checksum then
            Error "Block checksum mismatch"
          else
            (* Create block and populate *)
            let block = create () in
            block.data_pos <- data_size;
            
            (* Copy data *)
            let block_data = Cell.get block.data in
            Bytes.blit data 0 block_data 0 data_size;
            
            (* Read entry table *)
            let entry_start = header_size in
            for i = 0 to entry_count - 1 do
              let offset = entry_start + (i * 12) in
              let key_offset = Int32.to_int (Bytes.get_int32_be buf offset) in
              let value_offset = Int32.to_int (Bytes.get_int32_be buf (offset + 4)) in
              let value_size = Int32.to_int (Bytes.get_int32_be buf (offset + 8)) in
              Vector.push block.entries { key_offset; value_offset; value_size };
            done;
            
            Ok block
