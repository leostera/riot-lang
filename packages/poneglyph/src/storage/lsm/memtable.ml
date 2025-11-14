(** Memtable - In-memory sorted write buffer for LSM storage *)

open Std
open Std.Collections
open Std.Sync

module Bytes = Kernel.IO.Bytes
module SSTable = Sstable

(** Internal entry structure *)
type entry = {
  key : bytes;
  value : bytes;
}

(** Memtable structure
    
    Invariant: entries vector is always sorted by key
*)
type t = {
  entries : entry Vector.t;
  size_bytes : int Cell.t;
  max_size : int;
}

let create ~max_size =
  {
    entries = Vector.create ();
    size_bytes = cell 0;
    max_size;
  }

let size_bytes t = Cell.get t.size_bytes

let count t = Vector.len t.entries

let is_full t = size_bytes t >= t.max_size

(** Binary search for key index
    
    Returns Some(index) if key is found, None otherwise
*)
let find_index t key =
  let rec search low high =
    if low > high then None
    else
      let mid = low + (high - low) / 2 in
      let entry = Vector.get t.entries mid |> Option.expect ~msg:"mid in range" in
      
      match Bytes.compare key entry.key with
      | 0 -> Some mid
      | n when n < 0 -> search low (mid - 1)
      | _ -> search (mid + 1) high
  in
  
  if count t = 0 then None
  else search 0 (count t - 1)

let get t ~key =
  if Bytes.length key != 41 then None
  else
    match find_index t key with
    | None -> None
    | Some idx ->
        let entry = Vector.get t.entries idx |> Option.expect ~msg:"found index valid" in
        Some entry.value

let add t ~key ~value =
  if Bytes.length key != 41 then
    Error "Key must be exactly 41 bytes"
  else
    let entry_size = 41 + Bytes.length value in
    
    (* Check if key exists (for overwrite) *)
    match find_index t key with
    | Some idx ->
        (* Overwrite existing entry *)
        let old_entry = Vector.get t.entries idx |> Option.expect ~msg:"found index valid" in
        let old_size = 41 + Bytes.length old_entry.value in
        let new_size = size_bytes t - old_size + entry_size in
        
        if new_size > t.max_size then
          Error "Would exceed max_size"
        else (
          Vector.set t.entries idx { key; value };
          Cell.set t.size_bytes new_size;
          Ok ()
        )
    
    | None ->
        (* Insert new entry *)
        if size_bytes t + entry_size > t.max_size then
          Error "Would exceed max_size"
        else (
          Vector.push t.entries { key; value };
          (* Maintain sorted order *)
          Vector.sort_by t.entries (fun a b -> Bytes.compare a.key b.key);
          Cell.set t.size_bytes (size_bytes t + entry_size);
          Ok ()
        )

let iter t ~f =
  Vector.iter (fun entry -> f ~key:entry.key ~value:entry.value) t.entries

let fold t ~init ~f =
  let acc = cell init in
  iter t ~f:(fun ~key ~value ->
    Cell.set acc (f ~acc:(Cell.get acc) ~key ~value)
  );
  Cell.get acc

let flush_to_sstable t ~path =
  if count t = 0 then
    Ok 0
  else
    let builder = cell (SSTable.create_builder ~path) in
    let result = cell (Ok 0) in
    
    iter t ~f:(fun ~key ~value ->
      match Cell.get result with
      | Error _ -> ()  (* Already failed, skip rest *)
      | Ok _ ->
          match SSTable.add (Cell.get builder) ~key ~value with
          | Error e -> Cell.set result (Error e)
          | Ok b ->
              Cell.set builder b;
              Cell.set result (Ok (match Cell.get result with Ok n -> n + 1 | _ -> 0))
    );
    
    match Cell.get result with
    | Error e -> Error e
    | Ok entry_count ->
        match SSTable.finalize (Cell.get builder) with
        | Error e -> Error e
        | Ok _ -> Ok entry_count

let clear t =
  Vector.clear t.entries;
  Cell.set t.size_bytes 0
