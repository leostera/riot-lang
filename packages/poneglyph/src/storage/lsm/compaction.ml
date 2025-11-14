(** Compaction - Merge multiple SSTables to reduce read amplification *)

open Std
open Std.IO
open Std.Collections
open Std.Sync

(** Entry in the merge heap *)
type merge_entry = {
  key : bytes;
  value : bytes;
  priority : int;  (* SSTable index - higher = more recent *)
  sstable_index : int;  (* Which SSTable this came from *)
}

(** Compare entries for heap ordering: sort by key, break ties by priority (descending) *)
let compare_entries e1 e2 =
  match Bytes.compare e1.key e2.key with
  | 0 -> Int.compare e2.priority e1.priority  (* Higher priority first *)
  | c -> c

(** SSTable iterator state *)
type sstable_iter = {
  reader : Sstable.reader;
  entries : (bytes * bytes) Vector.t;
  mutable position : int;
}

(** Create an iterator for an SSTable *)
let create_iter path =
  match Sstable.open_read ~path with
  | Error err -> Error err
  | Ok reader ->
      let entries = vec [] in
      Sstable.iter reader ~f:(fun ~key ~value ->
          Vector.push entries (key, value));
      Ok { reader; entries; position = 0 }

(** Get next entry from iterator *)
let next_entry iter =
  if iter.position >= Vector.len iter.entries then None
  else
    match Vector.get iter.entries iter.position with
    | None -> None
    | Some (key, value) ->
        iter.position <- iter.position + 1;
        Some (key, value)

(** Close an iterator *)
let close_iter iter = Sstable.close iter.reader

(** Merge multiple SSTables into one *)
let merge_sstables ~inputs ~output =
  if List.length inputs = 0 then Error "No input SSTables provided"
  else
    (* Open all input SSTables *)
    let rec open_all paths index acc =
      match paths with
      | [] -> Ok (List.rev acc)
      | path :: rest -> (
          match create_iter path with
          | Error err ->
              (* Close already opened iterators *)
              List.iter close_iter acc;
              Error ("Failed to open " ^ path ^ ": " ^ err)
          | Ok iter -> open_all rest (index + 1) (iter :: acc))
    in

    match open_all inputs 0 [] with
    | Error err -> Error err
    | Ok iters ->
        (* Initialize heap with first entry from each SSTable *)
        let heap = vec [] in
        List.iteri
          (fun index iter ->
            match next_entry iter with
            | None -> () (* Empty SSTable *)
            | Some (key, value) ->
                Vector.push heap
                  { key; value; priority = index; sstable_index = index })
          iters;

        (* Sort heap *)
        Vector.sort_by heap compare_entries;

        (* Create output SSTable *)
        let writer = Sstable.create_builder ~path:output in
        let last_key = cell None in
        let current_writer = cell writer in

        (* Main merge loop *)
        let rec merge_loop () =
          if Vector.len heap = 0 then Ok ()
          else
            (* Pop minimum entry - heap should never be empty here due to len check *)
            let min_entry =
              match Vector.get heap 0 with
              | Some e -> e
              | None -> assert false  (* Unreachable: len > 0 *)
            in
            let rest = vec [] in
            for i = 1 to Vector.len heap - 1 do
              match Vector.get heap i with
              | Some e -> Vector.push rest e
              | None -> ()
            done;

            (* Write entry if it's a new key (deduplication) and not a tombstone *)
            let is_tombstone = Bytes.length min_entry.value = 0 in
            let should_write =
              (not is_tombstone) &&
              (match Cell.get last_key with
              | None -> true
              | Some prev_key -> not (Bytes.equal prev_key min_entry.key))
            in

            let write_result =
              if should_write then (
                Cell.set last_key (Some min_entry.key);
                match
                  Sstable.add (Cell.get current_writer) ~key:min_entry.key
                    ~value:min_entry.value
                with
                | Error err -> Error err
                | Ok new_writer ->
                    Cell.set current_writer new_writer;
                    Ok ())
              else (
                (* Still update last_key even if we skip tombstone to avoid duplicates *)
                Cell.set last_key (Some min_entry.key);
                Ok ())
            in

            match write_result with
            | Error err ->
                List.iter close_iter iters;
                Error ("Failed to write entry: " ^ err)
            | Ok () ->
                (* Get next entry from the same SSTable *)
                let iter = List.nth iters min_entry.sstable_index in
                (match next_entry iter with
                | None -> () (* SSTable exhausted *)
                | Some (key, value) ->
                    Vector.push rest
                      {
                        key;
                        value;
                        priority = min_entry.priority;
                        sstable_index = min_entry.sstable_index;
                      });

                (* Re-sort heap *)
                Vector.sort_by rest compare_entries;

                (* Update heap for next iteration *)
                let new_heap = rest in
                Vector.clear heap;
                for i = 0 to Vector.len new_heap - 1 do
                  match Vector.get new_heap i with
                  | Some e -> Vector.push heap e
                  | None -> ()
                done;

                merge_loop ()
        in

        match merge_loop () with
        | Error err ->
            List.iter close_iter iters;
            Error err
        | Ok () -> (
            match Sstable.finalize (Cell.get current_writer) with
            | Error err ->
                List.iter close_iter iters;
                Error ("Failed to finalize output SSTable: " ^ err)
            | Ok _ ->
                List.iter close_iter iters;
                Ok ())

(** Compact with optional deletion of inputs *)
let compact ~inputs ~output ~delete_inputs =
  match merge_sstables ~inputs ~output with
  | Error err -> Error err
  | Ok () ->
      if delete_inputs then
        (* Delete input SSTables *)
        let rec delete_all paths =
          match paths with
          | [] -> Ok ()
          | path :: rest -> (
              match Fs.remove_file (Path.v path) with
              | Error _ ->
                  Error ("Failed to delete " ^ path)
              | Ok () -> delete_all rest)
        in
        delete_all inputs
      else Ok ()
