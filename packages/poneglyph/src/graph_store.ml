open Std
open Std.Collections
open Model

type storage_impl =
  | Inmemory of Storage.Inmemory.t
  | SimpleFile of Storage.Simple_file.t
  | LsmStore of Storage.Lsm.Multi_store.t

type t = {
  storage : storage_impl;
  mutable filename : string option;
  mutable lock : Storage.Lsm.Lockfile.t option;
}

(* Generate unique LSM directory for test isolation *)
let default_lsm_dir () =
  let uuid = UUID.v7_monotonic () |> UUID.to_string in
  "/tmp/poneglyph_" ^ uuid

let open_shared ~data_dir =
  (* Acquire shared lock first *)
  let data_path = Path.v data_dir in
  match Storage.Lsm.Lockfile.acquire ~data_dir:data_path ~mode:Shared ~timeout:(Time.Duration.from_secs 30) with
  | Error e -> Error e
  | Ok lock ->
      (* Open store *)
      match Storage.Lsm.Multi_store.create ~data_dir with
      | Error e ->
          (* Failed to open, release lock *)
          ignore (Storage.Lsm.Lockfile.release lock);
          Error e
      | Ok store ->
          Ok { storage = LsmStore store; filename = None; lock = Some lock }

let open_exclusive ~data_dir ?(timeout = Time.Duration.from_secs 30) () =
  (* Acquire exclusive lock first *)
  let data_path = Path.v data_dir in
  match Storage.Lsm.Lockfile.acquire ~data_dir:data_path ~mode:Exclusive ~timeout with
  | Error e -> Error e
  | Ok lock ->
      (* Open store *)
      match Storage.Lsm.Multi_store.create ~data_dir with
      | Error e ->
          (* Failed to open, release lock *)
          ignore (Storage.Lsm.Lockfile.release lock);
          Error e
      | Ok store ->
          Ok { storage = LsmStore store; filename = None; lock = Some lock }

(* Legacy API for tests *)
type create_config =
  | InMemory
  | Persistent of string  (* File path *)
  | Lsm of string  (* Data directory *)

let create ?(config = Lsm (default_lsm_dir ())) () =
  match config with
  | InMemory -> 
      { storage = Inmemory (Storage.Inmemory.create ()); filename = None; lock = None }
  | Persistent path ->
      let store = Storage.Simple_file.load path in
      { storage = SimpleFile store; filename = Some path; lock = None }
  | Lsm data_dir ->
      open_exclusive ~data_dir ()
        |> Result.expect ~msg:"Failed to open LSM store"

(* Legacy API compatibility *)
let create_legacy ?persistent () =
  match persistent with
  | None -> create ~config:InMemory ()
  | Some path -> create ~config:(Persistent path) ()

let state graph facts =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.state store facts
  | SimpleFile store -> Storage.Simple_file.state store facts
  | LsmStore store -> 
      let count = Storage.Lsm.Multi_store.state store facts
        |> Result.expect ~msg:"LSM state failed" in
      (* Flush to ensure durability - high-level API guarantees data is on disk *)
      let _ = Storage.Lsm.Multi_store.flush_all store
        |> Result.expect ~msg:"LSM flush failed" in
      count

let retract graph ~fact_uri =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.retract store ~fact_uri
  | SimpleFile store -> Storage.Simple_file.retract store ~fact_uri
  | LsmStore store ->
      (* TODO: Need to implement retract by fact_uri for LSM *)
      (* For MVP: Find the fact first, then retract *)
      (* This requires scanning - not efficient, but works *)
      Log.warn "LSM retract by fact_uri not yet implemented - use retract with full fact";
      ()

let get graph ~entity ~attr =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get store ~entity ~attr
  | SimpleFile store -> Storage.Simple_file.get store ~entity ~attr
  | LsmStore store ->
      let facts = Storage.Lsm.Multi_store.get_entity_facts store ~entity in
      Iter.MutIterator.find facts ~fn:(fun f -> Uri.equal f.Fact.attribute attr)
      |> Option.map (fun f -> f.Fact.value)

let get_all_facts graph ~entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_all_facts store ~entity
  | SimpleFile store -> Storage.Simple_file.get_all_facts store ~entity
  | LsmStore store -> Storage.Lsm.Multi_store.get_entity_facts store ~entity

let get_current_facts graph ~entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_current_facts store ~entity
  | SimpleFile store -> Storage.Simple_file.get_current_facts store ~entity
  | LsmStore store -> Storage.Lsm.Multi_store.get_entity_facts store ~entity

let exists graph entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.exists store entity
  | SimpleFile store -> Storage.Simple_file.exists store entity
  | LsmStore store ->
      let facts = Storage.Lsm.Multi_store.get_entity_facts store ~entity in
      Iter.MutIterator.any facts ~fn:(fun _ -> true)

let get_kind graph entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_kind store entity
  | SimpleFile store -> Storage.Simple_file.get_kind store entity
  | LsmStore store ->
      let instance_of_attr = Uri.of_string "@field:instance_of" in
      let facts = Storage.Lsm.Multi_store.get_entity_facts store ~entity in
      (match Iter.MutIterator.find facts ~fn:(fun f -> Uri.equal f.Fact.attribute instance_of_attr) with
       | None -> None
       | Some f -> (match f.Fact.value with Fact.Uri k -> Some k | _ -> None))

let list_schemas graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.list_schemas store
  | SimpleFile store -> Storage.Simple_file.list_schemas store
  | LsmStore _store ->
      (* TODO: Implement schema listing for LSM *)
      (* Need to scan EAVT for @field:is_schema attribute *)
      Log.warn "LSM list_schemas not yet implemented";
      (* Return empty iterator *)
      let module EmptyIter = struct
        type state = unit
        type item = Uri.t
        let next () = None
        let size () = 0
        let clone () = ()
      end in
      Iter.MutIterator.make (module EmptyIter) ()

let save graph =
  match (graph.storage, graph.filename) with
  | SimpleFile store, Some path -> Storage.Simple_file.save store path
  | Inmemory _, Some path ->
      Log.warn ("Cannot save in-memory graph to " ^ path)
  | LsmStore _, _ ->
      (* LSM automatically persists to disk, no explicit save needed *)
      ()
  | _, None -> ()

let flush graph =
  match graph.storage with
  | Inmemory _ -> ()
  | SimpleFile _ -> ()
  | LsmStore store ->
      Storage.Lsm.Multi_store.flush_all store
      |> Result.expect ~msg:"Failed to flush LSM store"

let compact_if_needed graph ~threshold =
  match graph.storage with
  | Inmemory _ -> ()
  | SimpleFile _ -> ()
  | LsmStore store ->
      (* Check if tier 0 needs compaction *)
      if Storage.Lsm.Multi_store.needs_compaction store ~threshold then begin
        Log.info "CLI: Tier 0 exceeds threshold, compacting...";
        
        (* TRIGGER compaction (library does the work) *)
        match Storage.Lsm.Multi_store.compact_tier store ~tier:0 ~threshold () with
        | Ok true -> Log.info "CLI: Compaction completed"
        | Ok false -> ()  (* Nothing to compact *)
        | Error e -> Log.warn ("CLI: Compaction failed: " ^ e)
      end

let compact_tier graph ~tier ~threshold ?(max_merge=4) () =
  match graph.storage with
  | Inmemory _ -> Error "Not using LSM storage"
  | SimpleFile _ -> Error "Not using LSM storage"
  | LsmStore store -> Storage.Lsm.Multi_store.compact_tier store ~tier ~threshold ~max_merge ()

let close graph =
  (* Close storage *)
  (match graph.storage with
  | Inmemory _ -> ()
  | SimpleFile _ -> ()
  | LsmStore store ->
      Storage.Lsm.Multi_store.close store
      |> Result.expect ~msg:"Failed to close LSM store");
  
  (* Release lock if we have one *)
  match graph.lock with
  | None -> ()
  | Some lock ->
      graph.lock <- None;
      Storage.Lsm.Lockfile.release lock
      |> Result.expect ~msg:"Failed to release lock"

let transitive graph ~start ~edge ~max_depth =
  (* Lazy BFS iterator - explores graph on-demand *)
  let module BfsIter = struct
    type queue_item = { uri : Uri.t; depth : int }
    
    type state = {
      graph : t;
      edge : Uri.t;
      max_depth : int option;
      visited : (Uri.t, bool) HashMap.t;
      queue : queue_item Queue.t;
    }
    
    type item = Uri.t
    
    let rec next state =
      match Queue.pop state.queue with
      | None -> None
      | Some current ->
          (* Expand neighbors if within depth limit *)
          (match state.max_depth with
          | Some max when current.depth >= max -> ()
          | _ ->
              (* Get all facts for current entity *)
              let facts = get_current_facts state.graph ~entity:current.uri in
              
              (* Find neighbors via edge attribute *)
              Iter.MutIterator.for_each facts ~fn:(fun fact ->
                if Uri.equal fact.Fact.attribute state.edge then
                  match fact.Fact.value with
                  | Fact.Uri neighbor ->
                      if not (HashMap.contains_key state.visited neighbor) then begin
                        let _ = HashMap.insert state.visited neighbor true in
                        Queue.push state.queue { uri = neighbor; depth = current.depth + 1 }
                      end
                  | _ -> ())
          );
          
          (* Return current node *)
          Some current.uri
    
    let size state = Queue.len state.queue
    
    let clone state =
      (* Create new visited set with same entries *)
      let new_visited = HashMap.create () in
      HashMap.iter (fun k v -> let _ = HashMap.insert new_visited k v in ()) state.visited;
      
      (* Create new queue with same items *)
      let new_queue = Queue.create () in
      let items = Queue.to_list state.queue in
      List.iter (fun item -> Queue.push new_queue item) items;
      
      {
        graph = state.graph;
        edge = state.edge;
        max_depth = state.max_depth;
        visited = new_visited;
        queue = new_queue;
      }
  end in
  
  (* Initialize BFS state *)
  let visited = HashMap.create () in
  let _ = HashMap.insert visited start true in
  let queue = Queue.create () in
  Queue.push queue { BfsIter.uri = start; depth = 0 };
  
  let state = {
    BfsIter.graph;
    edge;
    max_depth;
    visited;
    queue;
  } in
  
  Iter.MutIterator.make (module BfsIter) state

let count_entities graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.entity_count store
  | SimpleFile store -> Storage.Simple_file.entity_count store
  | LsmStore store ->
      (* Count unique entities from all current facts *)
      let facts = Storage.Lsm.Multi_store.get_all_current_facts store in
      let entities = HashMap.create () in
      Iter.MutIterator.for_each facts ~fn:(fun fact ->
        let _ = HashMap.insert entities fact.Fact.entity () in
        ()
      );
      HashMap.len entities

let count_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.fact_count store
  | SimpleFile store -> Storage.Simple_file.fact_count store
  | LsmStore _store ->
      (* TODO: Fact counting for LSM *)
      (* Use get_detailed_stats for more accurate info *)
      0

let count_current_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.current_fact_count store
  | SimpleFile store -> Storage.Simple_file.current_fact_count store
  | LsmStore store ->
      let facts = Storage.Lsm.Multi_store.get_all_current_facts store in
      Iter.MutIterator.count facts

let get_detailed_stats graph =
  match graph.storage with
  | Inmemory _store -> 
      Data.Json.obj [("backend", Data.Json.string "inmemory")]
  | SimpleFile _store -> 
      Data.Json.obj [("backend", Data.Json.string "file")]
  | LsmStore store -> 
      Storage.Lsm.Multi_store.get_stats store

let find_entities graph ~attr ~value =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.find_entities_by_attr_value store ~attr ~value
  | SimpleFile store -> Storage.Simple_file.find_entities_by_attr_value store ~attr ~value
  | LsmStore store ->
      Storage.Lsm.Multi_store.find_entities_by_attr_value store ~attribute:attr ~value

let find_by_kind graph ~kind =
  let instance_of_attr = Uri.of_string "@field:instance_of" in
  find_entities graph ~attr:instance_of_attr ~value:(Fact.Uri kind)

let get_all_current_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_all_current_facts store
  | SimpleFile store -> Storage.Simple_file.get_all_current_facts store
  | LsmStore store ->
      (* Warning: This scans the entire EAVT index - expensive for large databases *)
      Log.debug "LSM get_all_current_facts: scanning entire database";
      Storage.Lsm.Multi_store.get_all_current_facts store

let get_facts_by_attribute graph ~attribute =
  match graph.storage with
  | Inmemory store ->
      (* Fallback: filter all facts *)
      let all_facts = Storage.Inmemory.get_all_current_facts store in
      Iter.MutIterator.filter all_facts ~fn:(fun fact -> Uri.equal fact.Fact.attribute attribute)
  | SimpleFile store ->
      (* Fallback: filter all facts *)
      let all_facts = Storage.Simple_file.get_all_current_facts store in
      Iter.MutIterator.filter all_facts ~fn:(fun fact -> Uri.equal fact.Fact.attribute attribute)
  | LsmStore store ->
      (* Optimized: Use AVET index for fast attribute lookup *)
      Storage.Lsm.Multi_store.get_facts_by_attribute store ~attribute

let find_by_source graph ~source =
  match graph.storage with
  | Inmemory store ->
      (* Fallback: filter all facts *)
      let all_facts = Storage.Inmemory.get_all_current_facts store in
      let entities = all_facts
        |> Iter.MutIterator.filter_map ~fn:(fun fact ->
            if Uri.equal fact.Fact.source_uri source then
              Some fact.Fact.entity
            else None)
        |> Iter.MutIterator.to_list
        |> List.sort_uniq Uri.compare in
      
      (* Convert to iterator *)
      let module ListIter = struct
        type state = { mutable remaining : Uri.t list }
        type item = Uri.t
        let next state = match state.remaining with
          | [] -> None
          | x :: xs -> state.remaining <- xs; Some x
        let size state = List.length state.remaining
        let clone state = { remaining = state.remaining }
      end in
      Iter.MutIterator.make (module ListIter) { remaining = entities }
  | SimpleFile store ->
      (* Fallback: filter all facts *)
      let all_facts = Storage.Simple_file.get_all_current_facts store in
      let entities = all_facts
        |> Iter.MutIterator.filter_map ~fn:(fun fact ->
            if Uri.equal fact.Fact.source_uri source then
              Some fact.Fact.entity
            else None)
        |> Iter.MutIterator.to_list
        |> List.sort_uniq Uri.compare in
      
      (* Convert to iterator *)
      let module ListIter = struct
        type state = { mutable remaining : Uri.t list }
        type item = Uri.t
        let next state = match state.remaining with
          | [] -> None
          | x :: xs -> state.remaining <- xs; Some x
        let size state = List.length state.remaining
        let clone state = { remaining = state.remaining }
      end in
      Iter.MutIterator.make (module ListIter) { remaining = entities }
  | LsmStore store ->
      (* Optimized: Use SOURCE index for fast lookup *)
      let facts = Storage.Lsm.Multi_store.get_facts_by_source store ~source in
      
      (* Collect unique entities *)
      let seen = HashMap.create () in
      let entities = vec [] in
      
      Iter.MutIterator.for_each facts ~fn:(fun fact ->
        let entity_str = Uri.to_string fact.Fact.entity in
        match HashMap.get seen entity_str with
        | Some _ -> ()  (* Already seen *)
        | None ->
            let _ = HashMap.insert seen entity_str () in
            Vector.push entities fact.Fact.entity
      );
      
      Vector.to_mut_iter entities

let retract_by_source graph ~source =
  match graph.storage with
  | Inmemory store ->
      (* Fallback: scan all facts *)
      let all_facts = Storage.Inmemory.get_all_current_facts store in
      Iter.MutIterator.for_each all_facts ~fn:(fun fact ->
        if Uri.equal fact.Fact.source_uri source then
          retract graph ~fact_uri:fact.Fact.fact_uri)
  | SimpleFile store ->
      (* Fallback: scan all facts *)
      let all_facts = Storage.Simple_file.get_all_current_facts store in
      Iter.MutIterator.for_each all_facts ~fn:(fun fact ->
        if Uri.equal fact.Fact.source_uri source then
          retract graph ~fact_uri:fact.Fact.fact_uri)
  | LsmStore store ->
      (* Optimized: Use SOURCE index for fast lookup *)
      let facts = Storage.Lsm.Multi_store.get_facts_by_source store ~source in
      Iter.MutIterator.for_each facts ~fn:(fun fact ->
        retract graph ~fact_uri:fact.Fact.fact_uri)

let cleanup_orphaned_files graph =
  match graph.storage with
  | Inmemory _ -> ()  (* No files to clean up *)
  | SimpleFile _ -> ()  (* TODO: implement for simple file backend *)
  | LsmStore store ->
      Storage.Lsm.Multi_store.cleanup_orphaned_files store
