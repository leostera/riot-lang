(** LSM Engine - Orchestrates all LSM components *)

open Std
open Std.Collections
open Std.Sync

module Bytes = Kernel.IO.Bytes

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
}

type stats = { memtable_size : int; sstable_count : int }

(* Helper: Generate next SSTable filename *)
let next_sstable_path engine =
  let id = Cell.get engine.next_sstable_id in
  Cell.set engine.next_sstable_id (id + 1);
  engine.config.data_dir ^ "/sstable_" ^ string_of_int id ^ ".sst"

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

(* Helper: Extract max SSTable ID from filenames *)
let max_sstable_id sstables =
  let rec find_max current paths =
    match paths with
    | [] -> current
    | path :: rest ->
        (* Extract ID from "sstable_NNN.sst" *)
        let basename = Path.basename (Path.v path) in
        if String.length basename > 13 then  (* "sstable_" = 8, ".sst" = 4, at least 1 digit *)
          let id_part = String.sub basename 8 (String.length basename - 12) in
          let id = try int_of_string id_part with _ -> 0 in
          find_max (max current id) rest
        else
          find_max current rest
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
      let wal_result =
        match Fs.exists (Path.v wal_path) with
        | Ok true -> Wal.open_existing ~path:wal_path
        | Ok false -> Wal.create ~path:wal_path
        | Error _ -> Wal.create ~path:wal_path  (* Create on error *)
      in

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
              (* Discover existing SSTables *)
              let sstables = discover_sstables config.data_dir in
              let next_id = max_sstable_id sstables + 1 in

              let engine =
                {
                  config;
                  memtable;
                  wal;
                  sstables = cell sstables;
                  next_sstable_id = cell next_id;
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

and flush engine =
  (* Only flush if memtable has data *)
  if Memtable.size_bytes engine.memtable = 0 then Ok ()
  else
    (* 1. Generate new SSTable path *)
    let sstable_path = next_sstable_path engine in

    (* 2. Flush memtable to SSTable *)
    (match Memtable.flush_to_sstable engine.memtable ~path:sstable_path with
    | Error e -> Error ("Memtable flush failed: " ^ e)
    | Ok _entry_count -> (
        (* 3. Truncate WAL *)
        match Wal.truncate engine.wal with
        | Error e -> Error ("WAL truncate failed: " ^ e)
        | Ok () ->
            (* 4. Clear memtable *)
            Memtable.clear engine.memtable;
            (* 5. Add SSTable to list (prepend since newest first) *)
            let current_sstables = Cell.get engine.sstables in
            Cell.set engine.sstables (sstable_path :: current_sstables);
            Ok ()))

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

let stats engine =
  {
    memtable_size = Memtable.size_bytes engine.memtable;
    sstable_count = List.length (Cell.get engine.sstables);
  }
