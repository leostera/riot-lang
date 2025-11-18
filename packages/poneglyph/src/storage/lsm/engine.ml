(** LSM Engine - Orchestrates all LSM components *)

open Std
open Std.Collections
open Std.Sync

module Bytes = Kernel.IO.Bytes

(** Manifest versioning for snapshot isolation
    
    Each version represents a point-in-time view of the manifest.
    Readers acquire snapshots (refcount++), writers create new versions.
    Old SSTables are deleted when their version's refcount reaches 0.
*)
module ManifestVersion = struct
  type t = {
    id : int;
    tables : Manifest.sstable_metadata list;
    refcount : int Cell.t;
    obsolete : string list Cell.t;  (* Paths to delete when refcount=0 *)
  }
  
  let create ~id ~tables =
    {
      id;
      tables;
      refcount = cell 0;
      obsolete = cell [];
    }
  
  (** Acquire a reference to this version (called by readers) *)
  let acquire version =
    let count = Cell.get version.refcount in
    Cell.set version.refcount (count + 1)
  
  (** Clean up obsolete files for a version.
      This is the equivalent of RocksDB's Version destructor.
      In C++, this happens automatically; in OCaml, we must call it manually. *)
  let cleanup version =
    let obsolete_files = Cell.get version.obsolete in
    if List.length obsolete_files > 0 then begin
      List.iter (fun path ->
        (* Check if file exists before trying to delete *)
        match Fs.exists (Path.v path) with
        | Ok true ->
            (match Fs.remove_file (Path.v path) with
            | Ok () -> ()
            | Error e ->
                Log.warn ("Failed to delete obsolete SSTable " ^ path ^ ": " ^ IO.error_message e))
        | Ok false ->
            (* File already deleted, skip silently *)
            ()
        | Error e ->
            Log.warn ("Failed to check existence of " ^ path ^ ": " ^ IO.error_message e)
      ) obsolete_files;
      
      (* Clear the list after deletion attempt *)
      Cell.set version.obsolete []
    end
  
  (** Release a reference to this version (called when reader finishes) *)
  let release version =
    let count = Cell.get version.refcount in
    Cell.set version.refcount (count - 1);
    
    (* Delete obsolete SSTables when refcount reaches 0 *)
    if count - 1 = 0 then
      cleanup version
  
  (** Mark SSTables as obsolete - they'll be deleted when refcount=0 *)
  let mark_obsolete version paths =
    let current = Cell.get version.obsolete in
    Cell.set version.obsolete (current @ paths)
end

type config = {
  data_dir : string;
  max_memtable_size : int;
  compaction_threshold : int;
}

type t = {
  config : config;
  memtable : Memtable.t;
  wal : Wal.t;
  sstables : string list Cell.t;  (* Mutable list of SSTable paths, newest first *)
  next_sstable_id : int Cell.t;  (* Counter for generating SSTable filenames *)
  manifest_path : string;  (* Path to manifest file *)
  manifest : Manifest.t Cell.t;  (* Manifest for SSTable metadata *)
  
  (* Snapshot isolation: versioned manifests with refcounting *)
  current_version : ManifestVersion.t Cell.t;  (* Current version for new readers *)
  all_versions : ManifestVersion.t list Cell.t;  (* All live versions (refcount > 0) *)
  next_version_id : int Cell.t;  (* Counter for version IDs *)
}

type stats = { memtable_size : int; sstable_count : int }

(* Helper: Generate next SSTable filename *)
let next_sstable_path engine =
  let id = Cell.get engine.next_sstable_id in
  Cell.set engine.next_sstable_id (id + 1);
  (* RocksDB-style: zero-padded 6-digit filenames for consistent sorting *)
  let id_str = string_of_int id in
  let padding = String.make (max 0 (6 - String.length id_str)) '0' in
  let filename = padding ^ id_str ^ ".sst" in
  engine.config.data_dir ^ "/" ^ filename

(* Helper: Discover existing SSTables in data directory *)
let discover_sstables data_dir =
  match Fs.read_dir (Path.v data_dir) with
  | Error _ -> []
  | Ok entries ->
      let sstables = Iter.MutIterator.to_list entries
        |> List.map Path.to_string
        |> List.filter (fun name -> 
            String.length name > 4 && 
            String.sub name (String.length name - 4) 4 = ".sst")
        |> List.map (fun basename -> data_dir ^ "/" ^ basename)  (* Construct full path *)
        |> List.sort (fun a b -> String.compare b a)  (* Reverse order - newest first *)
      in
      sstables

(* Helper: Parse SSTable filename to extract ID *)
let parse_sstable_id path =
  let basename = path |> Path.remove_extension |> Path.basename in
  
  (* Support both formats: *)
  (* Old: "sstable_123.sst" -> Some 123 *)
  (* New: "000123.sst" -> Some 123 *)
  match String.split_on_char '_' basename with
  | ["sstable"; id_str] ->
      (* Old format *)
      (try Some (int_of_string id_str) with _ -> None)
  | _ ->
      (* New format: pure number *)
      (try Some (int_of_string basename) with _ -> None)

(* Helper: Extract max SSTable ID from filenames *)
let max_sstable_id sstables =
  let rec find_max current paths =
    match paths with
    | [] -> current
    | path :: rest ->
        match parse_sstable_id (Path.v path) with
        | Some id -> find_max (max current id) rest
        | None -> find_max current rest
  in
  find_max 0 sstables

(* Helper: Replay WAL into memtable *)
let replay_wal wal memtable =
  match Wal.replay wal with
  | Error e -> Error ("Failed to replay WAL: " ^ e)
  | Ok entries ->
      let rec replay_entries entries =
        match entries with
        | [] -> Ok ()
        | entry :: rest -> (
            match entry with
            | Wal.Put (key, value) -> (
                match Memtable.add memtable ~key ~value with
                | Error e -> Error ("Failed to replay Put: " ^ e)
                | Ok () -> replay_entries rest)
            | Wal.Delete key -> (
                (* Use empty bytes as tombstone marker *)
                match Memtable.add memtable ~key ~value:(Bytes.create 0) with
                | Error e -> Error ("Failed to replay Delete: " ^ e)
                | Ok () -> replay_entries rest))
      in
      replay_entries entries

let rec open_engine config =
  (* Create data directory if it doesn't exist *)
  (match Fs.create_dir_all (Path.v config.data_dir) with
  | Error _ -> Error ("Failed to create data directory: " ^ config.data_dir)
  | Ok () -> (
      (* Open or create WAL *)
      let wal_path = config.data_dir ^ "/wal.log" in
      let wal_result = Wal.create_or_open ~path:wal_path in

      match wal_result with
      | Error e -> Error ("Failed to open WAL: " ^ e)
      | Ok wal -> (
          (* Create memtable *)
          let memtable = Memtable.create ~max_size:config.max_memtable_size in

          (* Replay WAL for crash recovery *)
          match replay_wal wal memtable with
          | Error e ->
              ignore (Wal.close wal);
              Error e
           | Ok () -> (
               (* Load or create manifest *)
               let manifest_path = config.data_dir ^ "/manifest.json" in
               let manifest = match Manifest.load ~path:manifest_path with
                 | Ok m -> m
                 | Error _ -> Manifest.empty ()
               in
               
               (* Get SSTables from manifest, or discover from directory *)
               let sstables_from_manifest = Manifest.get_sstables manifest ~index:"engine"
                 |> List.map (fun meta -> config.data_dir ^ "/" ^ meta.Manifest.path)
               in
               
               (* Use manifest if available, otherwise discover *)
               let sstables = if List.length sstables_from_manifest > 0
                 then sstables_from_manifest
                 else discover_sstables config.data_dir
               in
               
                (* RocksDB approach: read next_id from manifest, not directory scan *)
                let next_id = 
                  let manifest_next = Manifest.get_next_sstable_id manifest ~index:"engine" in
                  if manifest_next > 0 then
                    manifest_next  (* Use persisted value *)
                  else
                    (* Migration: old manifest without next_sstable_id *)
                    max_sstable_id sstables + 1
                in
                
                (* Initialize manifest versioning *)
                let initial_version = ManifestVersion.create
                  ~id:0
                  ~tables:(Manifest.get_sstables manifest ~index:"engine")
                in
                
                (* Acquire reference for the engine itself *)
                ManifestVersion.acquire initial_version;

                let engine =
                  {
                    config;
                    memtable;
                    wal;
                    sstables = cell sstables;
                    next_sstable_id = cell next_id;
                    manifest_path;
                    manifest = cell manifest;
                    current_version = cell initial_version;
                    all_versions = cell [initial_version];
                    next_version_id = cell 1;
                  }
                in

                Ok engine))))

and close engine =
  (* Flush memtable if it has data *)
  let flush_result =
    if Memtable.size_bytes engine.memtable > 0 then flush engine else Ok ()
  in

  match flush_result with
  | Error e -> Error e
  | Ok () -> (
      (* Release current version and cleanup all versions *)
      let current = Cell.get engine.current_version in
      ManifestVersion.release current;
      
      (* Force cleanup of all remaining versions *)
      let all = Cell.get engine.all_versions in
      List.iter (fun version ->
        (* Force cleanup even if refcount > 0, since we're closing *)
        ManifestVersion.cleanup version
      ) all;
      
      (* Close WAL *)
      match Wal.close engine.wal with
      | Error e -> Error ("Failed to close WAL: " ^ e)
      | Ok () -> Ok ())

and put engine ~key ~value =
  (* 1. Append to WAL first (durability) *)
  (match Wal.append engine.wal ~key ~value with
  | Error e -> Error ("WAL append failed: " ^ e)
  | Ok () -> (
      (* 2. Write to memtable *)
      match Memtable.add engine.memtable ~key ~value with
      | Error e -> Error ("Memtable add failed: " ^ e)
      | Ok () ->
          (* 3. Check if flush needed *)
          if Memtable.size_bytes engine.memtable >= engine.config.max_memtable_size
          then flush engine
          else Ok ()))

(** Batch put - MUCH faster than calling put repeatedly
    
    Performance: Instead of n × (WAL write + memtable sort), does:
    - 1 × batch WAL write
    - 1 × batch memtable add (single sort)
    
    Expected speedup: 50-70% for large batches
*)
and put_batch engine ~entries =
  (* 1. Batch append to WAL (single fsync) *)
  let wal_entries = List.map (fun (key, value) -> Wal.Put (key, value)) entries in
  (match Wal.append_batch engine.wal wal_entries with
  | Error e -> Error ("WAL batch append failed: " ^ e)
  | Ok () -> (
      (* 2. Batch add to memtable (single sort) *)
      match Memtable.add_batch engine.memtable ~entries with
      | Error e -> Error ("Memtable batch add failed: " ^ e)
      | Ok () ->
          (* 3. Check if flush needed *)
          if Memtable.size_bytes engine.memtable >= engine.config.max_memtable_size
          then flush engine
          else Ok ()))

and get engine ~key =
  (* 1. Check memtable first (most recent) *)
  match Memtable.get engine.memtable ~key with
  | Some value -> 
      (* Check if it's a tombstone (empty bytes = deleted) *)
      if Bytes.length value = 0 then None else Some value
  | None -> (
      (* 2. Check SSTables in order (already sorted newest first) *)
      let rec search_sstables tables =
        match tables with
        | [] -> None
        | path :: rest -> (
            match Sstable.open_read ~path with
            | Error _ -> search_sstables rest
            | Ok reader ->
                let result = Sstable.get reader ~key in
                Sstable.close reader;
                (match result with 
                 | Some value ->
                     (* Check if it's a tombstone *)
                     if Bytes.length value = 0 then None else Some value
                 | None -> search_sstables rest))
      in
      search_sstables (Cell.get engine.sstables))

and delete engine ~key =
  (* Deletes are handled as tombstones (empty bytes) *)
  match Wal.append_delete engine.wal ~key with
  | Error e -> Error ("WAL append_delete failed: " ^ e)
  | Ok () -> (
      (* Use empty bytes as tombstone marker *)
      match Memtable.add engine.memtable ~key ~value:(Bytes.create 0) with
      | Error e -> Error ("Memtable add tombstone failed: " ^ e)
      | Ok () ->
          (* Check if flush needed *)
          if Memtable.size_bytes engine.memtable >= engine.config.max_memtable_size
          then flush engine
          else Ok ())

and write_batch engine (ops : [> `Put of bytes * bytes | `Delete of bytes ] list) =
  (* Convert operations to WAL entries *)
  let wal_entries = List.map (fun (op : [> `Put of bytes * bytes | `Delete of bytes ]) ->
    match op with
    | `Put (key, value) -> Wal.Put (key, value)
    | `Delete key -> Wal.Delete key
    | _ -> panic "Unknown operation in write_batch"
  ) ops in
  
  (* 1. Append all to WAL atomically *)
  (match Wal.append_batch engine.wal wal_entries with
  | Error e -> Error ("WAL append_batch failed: " ^ e)
  | Ok () ->
      (* 2. Apply all to memtable *)
      let rec apply_to_memtable (ops : [> `Put of bytes * bytes | `Delete of bytes ] list) =
        match ops with
        | [] -> Ok ()
        | op :: rest -> (
            match op with
            | `Put (key, value) -> (
                match Memtable.add engine.memtable ~key ~value with
                | Error e -> Error ("Memtable batch add failed: " ^ e)
                | Ok () -> apply_to_memtable rest)
            | `Delete key -> (
                (* Use empty bytes as tombstone *)
                match Memtable.add engine.memtable ~key ~value:(Bytes.create 0) with
                | Error e -> Error ("Memtable batch delete failed: " ^ e)
                | Ok () -> apply_to_memtable rest)
            | _ -> panic "Unknown operation in apply_to_memtable")
      in
      
      match apply_to_memtable ops with
      | Error e -> Error e
      | Ok () ->
          (* 3. Check if flush needed *)
          if Memtable.size_bytes engine.memtable >= engine.config.max_memtable_size
          then flush engine
          else Ok ())

and flush engine =
  (* Only flush if memtable has data *)
  if Memtable.size_bytes engine.memtable = 0 then Ok ()
  else
    (* 1. Generate new SSTable path *)
    let sstable_path = next_sstable_path engine in

    (* 2. Flush memtable to SSTable *)
    (match Memtable.flush_to_sstable engine.memtable ~path:sstable_path with
    | Error e -> Error ("Memtable flush failed: " ^ e)
    | Ok entry_count -> (
        (* 3. Get SSTable metadata *)
        let file_meta_result = Fs.metadata (Path.v sstable_path) in
        let reader_result = Sstable.open_read ~path:sstable_path in
        
        (match (file_meta_result, reader_result) with
        | Error e, _ -> Error ("Failed to get SSTable metadata: " ^ IO.error_message e)
        | _, Error e -> Error ("Failed to open new SSTable: " ^ e)
        | Ok file_meta, Ok reader ->
            let first_key = Sstable.first_key reader in
            let last_key = Sstable.last_key reader in
            Sstable.close reader;
            
            (* Create manifest metadata *)
            let basename = Path.basename (Path.v sstable_path) in
            let file_size = Fs.Metadata.len file_meta in
            let sstable_meta = {
              Manifest.path = basename;
              tier = Manifest.tier_for_size file_size;
              size_bytes = file_size;
              min_key = first_key;
              max_key = last_key;
              entry_count;
              created_at = Time.SystemTime.now () |> Time.SystemTime.to_unix_timestamp |> Int64.of_int;
            } in
            
            (* Update manifest *)
            let manifest = Cell.get engine.manifest in
            let manifest' = Manifest.add_sstable manifest ~index:"engine" sstable_meta in
            
            (* Persist next_sstable_id to manifest (RocksDB approach) *)
            let manifest' = Manifest.update_next_sstable_id manifest' 
              ~index:"engine" 
              (Cell.get engine.next_sstable_id) in
            
            Cell.set engine.manifest manifest';
            
            (* Save manifest atomically *)
            (match Manifest.save ~path:engine.manifest_path manifest' with
            | Error e -> Error ("Failed to save manifest: " ^ e)
            | Ok () ->
                (* Create new manifest version for snapshot isolation *)
                let new_tables = Manifest.get_sstables manifest' ~index:"engine" in
                let version_id = Cell.get engine.next_version_id in
                Cell.set engine.next_version_id (version_id + 1);
                
                let new_version = ManifestVersion.create
                  ~id:version_id
                  ~tables:new_tables
                in
                
                (* Acquire reference for the engine before making it current *)
                ManifestVersion.acquire new_version;
                
                (* Atomically swap to new version *)
                let old_version = Cell.get engine.current_version in
                Cell.set engine.current_version new_version;
                
                (* Release old version - this decrements refcount and triggers cleanup if refcount=0 *)
                ManifestVersion.release old_version;
                
                (* Add new version to all_versions list *)
                let versions = Cell.get engine.all_versions in
                Cell.set engine.all_versions (new_version :: versions);
                
                (* Remove dead versions from list (cleanup already happened in release()) *)
                let live_versions = List.filter (fun v ->
                  Cell.get v.ManifestVersion.refcount > 0
                ) versions in
                Cell.set engine.all_versions live_versions;
                
                (* 4. Truncate WAL *)
                match Wal.truncate engine.wal with
                | Error e -> Error ("WAL truncate failed: " ^ e)
                | Ok () ->
                    (* 5. Clear memtable *)
                    Memtable.clear engine.memtable;
                    (* 6. Add SSTable to list (prepend since newest first) *)
                    let current_sstables = Cell.get engine.sstables in
                    Cell.set engine.sstables (sstable_path :: current_sstables);
                    Ok ()))))

let compact engine =
  let current_sstables = Cell.get engine.sstables in
  let sstable_count = List.length current_sstables in

  if sstable_count < engine.config.compaction_threshold then Ok ()
  else
    (* Take oldest N SSTables (from the end of the list) *)
    let n = engine.config.compaction_threshold in
    let rec take_last n lst =
      let len = List.length lst in
      if len <= n then lst
      else
        let rec drop n lst =
          match (n, lst) with 0, _ -> lst | _, [] -> [] | n, _ :: rest -> drop (n - 1) rest
        in
        drop (len - n) lst
    in

    let rec drop_last n lst =
      let len = List.length lst in
      if len <= n then []
      else
        let rec take n lst =
          match (n, lst) with 0, _ -> [] | _, [] -> [] | n, x :: rest -> x :: take (n - 1) rest
        in
        take (len - n) lst
    in

    let to_compact = take_last n current_sstables in
    let output_path = next_sstable_path engine in

    (* Merge SSTables *)
    match Compaction.compact ~inputs:to_compact ~output:output_path ~delete_inputs:true with
    | Error e -> Error ("Compaction failed: " ^ e)
    | Ok () ->
        (* Update SSTable list: remove old, add new *)
        let remaining = drop_last n current_sstables in
        Cell.set engine.sstables (output_path :: remaining);
        Ok ()

let needs_compaction engine =
  List.length (Cell.get engine.sstables) >= engine.config.compaction_threshold

let compact_one_tier engine ~tier ~threshold ?(max_merge=4) () =
  let manifest = Cell.get engine.manifest in
  let sstables = Manifest.get_sstables manifest ~index:"engine" in
  let by_tier = Manifest.group_by_tier sstables in
  
  match List.assoc_opt tier by_tier with
  | None -> Ok false  (* Tier doesn't exist *)
  | Some tier_sstables when List.length tier_sstables < threshold -> 
      Ok false  (* Below threshold *)
  | Some tier_sstables ->
      (* Calculate average file size to determine batch size *)
      let total_size = List.fold_left (fun acc meta -> 
        acc + meta.Manifest.size_bytes
      ) 0 tier_sstables in
      let avg_size = total_size / List.length tier_sstables in
      
      (* Size-based batching: smaller files = larger batches *)
      let batch_size = 
        if avg_size < 10_000 then min max_merge 50       (* < 10KB: up to 50 files *)
        else if avg_size < 100_000 then min max_merge 20  (* < 100KB: up to 20 files *)
        else min max_merge 10                             (* >= 100KB: up to 10 files *)
      in
      
      (* Pick N oldest SSTables to merge (sorted by created_at) *)
      let to_merge = List.sort (fun a b -> 
        Int64.compare a.Manifest.created_at b.Manifest.created_at
      ) tier_sstables
        |> (fun lst -> if List.length lst > batch_size then 
              let rec take n acc l = 
                match (n, l) with
                | (0, _) | (_, []) -> List.rev acc
                | (n, x :: xs) -> take (n - 1) (x :: acc) xs
              in
              take batch_size [] lst
            else lst)
      in
      
      let input_paths = List.map (fun meta -> 
        engine.config.data_dir ^ "/" ^ meta.Manifest.path
      ) to_merge in
      
      let output_path = next_sstable_path engine in
      
      (* Use existing Compaction.compact function *)
      (match Compaction.compact ~inputs:input_paths ~output:output_path ~delete_inputs:false with
      | Error e -> Error e
      | Ok () ->
          (* Get output metadata *)
          let output_meta_file_result = Fs.metadata (Path.v output_path) in
          let output_reader_result = Sstable.open_read ~path:output_path in
          
          (match (output_meta_file_result, output_reader_result) with
          | Error e, _ -> Error ("Failed to get output metadata: " ^ IO.error_message e)
          | _, Error e -> Error ("Failed to open output SSTable: " ^ e)
          | Ok output_meta_file, Ok reader ->
              let file_size = Fs.Metadata.len output_meta_file in
              let output_meta = {
                Manifest.path = Path.basename (Path.v output_path);
                tier = Manifest.tier_for_size file_size;  (* Assign tier based on file size *)
                size_bytes = file_size;
                min_key = Sstable.first_key reader;
                max_key = Sstable.last_key reader;
                entry_count = Sstable.entry_count reader;
                created_at = Time.SystemTime.now () |> Time.SystemTime.to_unix_timestamp |> Int64.of_int;
              } in
              Sstable.close reader;
              
              (* Update manifest: remove inputs, add output *)
              let manifest = Cell.get engine.manifest in
              let input_paths_rel = List.map (fun m -> m.Manifest.path) to_merge in
              let manifest_without_old = Manifest.remove_sstables manifest ~index:"engine" ~paths:input_paths_rel in
              let manifest' = Manifest.add_sstable manifest_without_old ~index:"engine" output_meta in
              
              (* Persist next_sstable_id to manifest (RocksDB approach) *)
              let manifest' = Manifest.update_next_sstable_id manifest' 
                ~index:"engine" 
                (Cell.get engine.next_sstable_id) in
              
              Cell.set engine.manifest manifest';
              
              (* Save manifest *)
              (match Manifest.save ~path:engine.manifest_path manifest' with
              | Error e -> Error ("Failed to save manifest: " ^ e)
              | Ok () ->
                  (* SNAPSHOT ISOLATION: Create new manifest version *)
                  let new_tables = Manifest.get_sstables manifest' ~index:"engine" in
                  let version_id = Cell.get engine.next_version_id in
                  Cell.set engine.next_version_id (version_id + 1);
                  
                  let new_version = ManifestVersion.create
                    ~id:version_id
                    ~tables:new_tables
                  in
                  
                  (* Acquire reference for the engine before making it current *)
                  ManifestVersion.acquire new_version;
                  
                  (* Mark old SSTables as obsolete in OLD version *)
                  let old_version = Cell.get engine.current_version in
                  ManifestVersion.mark_obsolete old_version input_paths;
                  
                  (* Atomically swap to new version *)
                  Cell.set engine.current_version new_version;
                  
                  (* Release old version - this decrements refcount and triggers cleanup if refcount=0 *)
                  ManifestVersion.release old_version;
                  
                  (* Add new version to all_versions list *)
                  let versions = Cell.get engine.all_versions in
                  Cell.set engine.all_versions (new_version :: versions);
                  
                  (* Remove dead versions from list (cleanup already happened in release()) *)
                  let live_versions = List.filter (fun v ->
                    Cell.get v.ManifestVersion.refcount > 0
                  ) versions in
                  Cell.set engine.all_versions live_versions;
                  
                  (* Update engine's SSTable list *)
                  let current = Cell.get engine.sstables in
                  let current' = List.filter (fun p -> 
                    not (List.mem p input_paths)
                  ) current in
                  Cell.set engine.sstables (output_path :: current');
                  
                  Ok true))  (* Compaction performed *)
      )

let stats engine =
  {
    memtable_size = Memtable.size_bytes engine.memtable;
    sstable_count = List.length (Cell.get engine.sstables);
  }

(** Scan all keys with given prefix
    
    Uses snapshot isolation: acquires a manifest version at start,
    releases it when iteration completes. This allows concurrent
    compaction without file-not-found errors.
*)
let scan_prefix engine ~prefix =
  (* SNAPSHOT ISOLATION: Acquire current manifest version *)
  let snapshot = Cell.get engine.current_version in
  ManifestVersion.acquire snapshot;
  
  (* Build SSTable paths from snapshot (not from engine.sstables!) *)
  let snapshot_sstables = List.map (fun (meta : Manifest.sstable_metadata) ->
    engine.config.data_dir ^ "/" ^ meta.path
  ) snapshot.tables in
  
  (* Shared state for deduplication across all sources *)
  let seen = HashMap.create () in
  
  (* Helper: Mark key as seen and check if it's new *)
  let mark_seen key =
    let key_str = Bytes.to_string key in
    match HashMap.get seen key_str with
    | Some _ -> false  (* Already seen *)
    | None ->
        let _ = HashMap.insert seen key_str true in
        true  (* New key *)
  in
  
  (* 1. Get lazy memtable iterator *)
  let memtable_iter = Memtable.scan_prefix engine.memtable ~prefix in
  
  (* Filter memtable results: mark as seen, filter tombstones *)
  let memtable_filtered = Iter.MutIterator.filter_map memtable_iter ~fn:(fun (key, value) ->
    let _ = mark_seen key in  (* Mark all memtable keys as seen *)
    if Bytes.length value > 0 then Some (key, value) else None
  ) in
  
  (* 2. Create lazy SSTable chain iterator *)
  
  (* Custom iterator module for on-demand SSTable scanning *)
  let module SStableChainIter = struct
    type state = {
      mutable remaining_paths : string list;
      mutable current_reader : Sstable.reader option;
      mutable current_results : (bytes * bytes) list;
      prefix : bytes;
      seen : (string, bool) HashMap.t;
    }
    
    type item = bytes * bytes
    
    let rec next state =
      match state.current_results with
      | result :: rest ->
          (* Have results from current SSTable *)
          state.current_results <- rest;
          Some result
      | [] ->
          (* Current SSTable exhausted, try next *)
          close_current_and_open_next state
    
    and close_current_and_open_next state =
      (* Close current reader if any *)
      (match state.current_reader with
       | Some reader -> Sstable.close reader
       | None -> ());
      state.current_reader <- None;
      
      (* Try to open next SSTable *)
      match state.remaining_paths with
      | [] -> None  (* No more SSTables *)
      | path :: rest ->
          state.remaining_paths <- rest;
          (match Sstable.open_read ~path with
          | Error _ -> 
              (* Failed to open, try next *)
              close_current_and_open_next state
          | Ok reader ->
              state.current_reader <- Some reader;
              (* Scan for matching keys *)
              let all_results = Sstable.scan_prefix reader ~prefix:state.prefix in
              (* Filter out already-seen keys and tombstones *)
              let filtered = List.filter (fun (key, value) ->
                let key_str = Bytes.to_string key in
                match HashMap.get state.seen key_str with
                | Some _ -> false  (* Already seen *)
                | None ->
                    let _ = HashMap.insert state.seen key_str true in
                    Bytes.length value > 0  (* Filter tombstones *)
              ) all_results in
              state.current_results <- filtered;
              next state)  (* Recurse to get first result *)
    
    let size state =
      (* Approximate: current results + unknown from remaining SSTables *)
      List.length state.current_results
    
    let clone state =
      {
        remaining_paths = state.remaining_paths;
        current_reader = state.current_reader;
        current_results = state.current_results;
        prefix = state.prefix;
        seen = state.seen;
      }
  end in
  
  let sstable_state = {
    SStableChainIter.remaining_paths = snapshot_sstables;  (* Use snapshot! *)
    current_reader = None;
    current_results = [];
    prefix;
    seen;
  } in
  let sstable_iter = Iter.MutIterator.make 
    (module SStableChainIter) 
    sstable_state in
  
  (* 3. Chain memtable and SSTable iterators *)
  let chained = Iter.MutIterator.chain memtable_filtered sstable_iter in
  
  (* 4. Wrap iterator to release snapshot when consumed/dropped *)
  let module SnapshotReleaseIter = struct
    type state = {
      inner : (bytes * bytes) Iter.MutIterator.t;
      snapshot : ManifestVersion.t;
      released : bool Cell.t;  (* Use Cell for proper mutability *)
    }
    
    type item = bytes * bytes
    
    let next state =
      match Iter.MutIterator.next state.inner with
      | Some item -> Some item
      | None ->
          (* Iterator exhausted - release snapshot *)
          if not (Cell.get state.released) then begin
            ManifestVersion.release state.snapshot;
            Cell.set state.released true
          end;
          None
    
    let size state = Iter.MutIterator.size state.inner
    
    let clone state = 
      (* Acquire additional reference for cloned iterator *)
      if not (Cell.get state.released) then
        ManifestVersion.acquire state.snapshot;
      {
        inner = Iter.MutIterator.clone state.inner;
        snapshot = state.snapshot;
        released = cell false;  (* New released flag for clone *)
      }
  end in
  
  let wrapped_state = {
    SnapshotReleaseIter.inner = chained;
    snapshot;
    released = cell false;
  } in
  
  Iter.MutIterator.make (module SnapshotReleaseIter) wrapped_state

(** Get current manifest *)
let get_manifest engine =
  Cell.get engine.manifest
