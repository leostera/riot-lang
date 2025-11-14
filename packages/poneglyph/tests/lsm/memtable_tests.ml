(** Tests for Memtable - Mixed Unit and Property Tests *)

open Std
open Std.Collections
open Std.Sync
open Propane
open Poneglyph

module Memtable = Poneglyph.Storage.Lsm.Memtable
module Key = Poneglyph.Storage.Lsm.Key
module Encoding = Poneglyph.Storage.Lsm.Encoding
module SSTable = Poneglyph.Storage.Lsm.Sstable
module Bytes = Kernel.IO.Bytes

(** {1 Helpers} *)

(** Create test EAVT key with given entity_id *)
let make_test_key entity_id =
  let key : Key.eavt_key =
    {
      entity_id;
      attr_id = 1L;
      value_kind = Encoding.VK_Int;
      value_repr = 42L;
      tx_id = 1L;
      fact_id = entity_id;
    }
  in
  Key.encode_eavt key

(** Create test value *)
let make_test_value str =
  Bytes.of_string str

(** {1 Unit Tests} *)

(** Test 1: Create empty memtable *)
let test_create_empty () =
  let mt = Memtable.create ~max_size:1000 in
  
  if Memtable.count mt != 0 then Error "Empty memtable should have count 0"
  else if Memtable.size_bytes mt != 0 then Error "Empty memtable should have size 0"
  else if Memtable.is_full mt then Error "Empty memtable should not be full"
  else Ok ()

(** Test 2: Add single entry *)
let test_add_single () =
  let mt = Memtable.create ~max_size:1000 in
  let key = make_test_key 1L in
  let value = make_test_value "test_value" in
  
  match Memtable.add mt ~key ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      if Memtable.count mt != 1 then Error "Count should be 1"
      else
        let expected_size = 41 + Bytes.length value in
        if Memtable.size_bytes mt != expected_size then
          Error ("Size mismatch: expected " ^ string_of_int expected_size ^
                " got " ^ string_of_int (Memtable.size_bytes mt))
        else Ok ()

(** Test 3: Add multiple entries *)
let test_add_multiple () =
  let mt = Memtable.create ~max_size:10000 in
  
  let rec add_entries i =
    if i > 10 then Ok ()
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match Memtable.add mt ~key ~value with
      | Error e -> Error e
      | Ok () -> add_entries (i + 1)
  in
  
  match add_entries 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok () ->
      if Memtable.count mt != 10 then
        Error ("Expected 10 entries, got " ^ string_of_int (Memtable.count mt))
      else Ok ()

(** Test 4: Get existing key *)
let test_get_existing () =
  let mt = Memtable.create ~max_size:1000 in
  let key = make_test_key 5L in
  let value = make_test_value "found_me" in
  
  match Memtable.add mt ~key ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      match Memtable.get mt ~key with
      | None -> Error "Failed to find key"
      | Some retrieved ->
          if Bytes.compare retrieved value != 0 then
            Error "Retrieved value doesn't match"
          else Ok ()

(** Test 5: Get non-existent key *)
let test_get_nonexistent () =
  let mt = Memtable.create ~max_size:1000 in
  let key1 = make_test_key 1L in
  let key2 = make_test_key 99L in
  let value = make_test_value "test" in
  
  match Memtable.add mt ~key:key1 ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      match Memtable.get mt ~key:key2 with
      | Some _ -> Error "Should not find non-existent key"
      | None -> Ok ()

(** Test 6: Size limit enforced *)
let test_size_limit () =
  let mt = Memtable.create ~max_size:100 in  (* Small limit *)
  
  let key = make_test_key 1L in
  let large_value = Bytes.create 200 in  (* Too large *)
  
  match Memtable.add mt ~key ~value:large_value with
  | Ok () -> Error "Should reject value that exceeds max_size"
  | Error _ -> Ok ()

(** Test 7: Iteration in sorted order *)
let test_iteration_sorted () =
  let mt = Memtable.create ~max_size:10000 in
  
  (* Add in random order *)
  let keys = [5; 2; 8; 1; 9; 3] in
  let rec add_keys ks =
    match ks with
    | [] -> Ok ()
    | k :: rest ->
        let key = make_test_key (Int64.of_int k) in
        let value = make_test_value (string_of_int k) in
        match Memtable.add mt ~key ~value with
        | Error e -> Error e
        | Ok () -> add_keys rest
  in
  
  match add_keys keys with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      (* Verify iteration is sorted *)
      let collected = cell [] in
      Memtable.iter mt ~f:(fun ~key ~value ->
        let k_bytes = Bytes.to_string value in
        let k = int_of_string k_bytes in
        Cell.set collected (k :: Cell.get collected)
      );
      
      let sorted = List.rev (Cell.get collected) in
      let expected = [1; 2; 3; 5; 8; 9] in
      
      if sorted != expected then
        Error ("Keys not sorted: got " ^ String.concat "," (List.map string_of_int sorted))
      else Ok ()

(** Test 8: Flush to SSTable *)
let test_flush_to_sstable () =
  let mt = Memtable.create ~max_size:10000 in
  let path = "/tmp/memtable_flush_test.sst" in
  
  (* Add some entries *)
  let rec add_entries i =
    if i > 5 then Ok ()
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("val_" ^ string_of_int i) in
      match Memtable.add mt ~key ~value with
      | Error e -> Error e
      | Ok () -> add_entries (i + 1)
  in
  
  match add_entries 1 with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      match Memtable.flush_to_sstable mt ~path with
      | Error e -> Error ("Flush failed: " ^ e)
      | Ok count ->
          if count != 5 then
            Error ("Expected 5 entries, flushed " ^ string_of_int count)
          else
            (* Verify SSTable was created *)
            match SSTable.open_read ~path with
            | Error e -> Error ("Failed to open SSTable: " ^ e)
            | Ok reader ->
                let sst_count = SSTable.entry_count reader in
                SSTable.close reader;
                let _ = Fs.remove_file (Path.v path) in
                
                if sst_count != 5 then
                  Error ("SSTable has wrong count: " ^ string_of_int sst_count)
                else Ok ()

(** Test 9: Clear memtable *)
let test_clear () =
  let mt = Memtable.create ~max_size:1000 in
  
  let key = make_test_key 1L in
  let value = make_test_value "test" in
  
  match Memtable.add mt ~key ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok () ->
      Memtable.clear mt;
      
      if Memtable.count mt != 0 then Error "Count should be 0 after clear"
      else if Memtable.size_bytes mt != 0 then Error "Size should be 0 after clear"
      else Ok ()

(** Test 10: Overwrite key (last write wins) *)
let test_overwrite () =
  let mt = Memtable.create ~max_size:1000 in
  let key = make_test_key 1L in
  let value1 = make_test_value "first" in
  let value2 = make_test_value "second" in
  
  match Memtable.add mt ~key ~value:value1 with
  | Error e -> Error ("First add failed: " ^ e)
  | Ok () ->
      match Memtable.add mt ~key ~value:value2 with
      | Error e -> Error ("Second add failed: " ^ e)
      | Ok () ->
          (* Should have only 1 entry *)
          if Memtable.count mt != 1 then
            Error ("Expected 1 entry after overwrite, got " ^ string_of_int (Memtable.count mt))
          else
            match Memtable.get mt ~key with
            | None -> Error "Key not found after overwrite"
            | Some retrieved ->
                if Bytes.compare retrieved value2 != 0 then
                  Error "Should get second value (last write wins)"
                else Ok ()

(** {1 Property Tests} *)

(** Generate random EAVT key *)
let arb_key =
  Arbitrary.make
    ~print:(fun entity_id -> "Key(" ^ Int64.to_string entity_id ^ ")")
    Generator.(map Int64.of_int (int_range 1 1000))

(** Generate random bytes value *)
let arb_value =
  Arbitrary.make
    ~print:(fun b -> "Value(" ^ string_of_int (Bytes.length b) ^ " bytes)")
    Generator.(map Bytes.of_string (string_size (int_range 5 20) char))

(** Property: Memtable maintains sorted order *)
let prop_sorted_invariant =
  property "Memtable maintains sorted order after any adds"
    Arbitrary.(list (pair arb_key arb_value))
    (fun pairs ->
      let mt = Memtable.create ~max_size:100000 in
      
      (* Add all pairs *)
      let rec add_all ps =
        match ps with
        | [] -> true
        | (entity_id, value) :: rest ->
            let key = make_test_key entity_id in
            match Memtable.add mt ~key ~value with
            | Error _ -> assume_fail ()  (* Skip if doesn't fit *)
            | Ok () -> add_all rest
      in
      
      if not (add_all pairs) then false
      else
        (* Verify iteration gives sorted keys *)
        let prev_key = cell None in
        let is_sorted = cell true in
        
        Memtable.iter mt ~f:(fun ~key ~value:_ ->
          match Cell.get prev_key with
          | None -> Cell.set prev_key (Some key)
          | Some pk ->
              if Bytes.compare pk key >= 0 then
                Cell.set is_sorted false;
              Cell.set prev_key (Some key)
        );
        
        Cell.get is_sorted
    )

(** Property: Get returns what was added *)
let prop_query_correctness =
  property "Get returns value that was added"
    Arbitrary.(pair arb_key arb_value)
    (fun (entity_id, value) ->
      let mt = Memtable.create ~max_size:10000 in
      let key = make_test_key entity_id in
      
      match Memtable.add mt ~key ~value with
      | Error _ -> assume_fail ()
      | Ok () ->
          match Memtable.get mt ~key with
          | None -> false
          | Some retrieved -> Bytes.compare retrieved value = 0
    )

(** Property: Flush preserves all data *)
let prop_flush_preserves_data =
  property "Flush to SSTable preserves all data"
    Arbitrary.(list (pair arb_key arb_value))
    (fun pairs ->
      (* Limit size *)
      let pairs = List.take 10 pairs |> List.sort_uniq (fun (a, _) (b, _) -> Int64.compare a b) in
      assume (List.length pairs > 0);
      
      let mt = Memtable.create ~max_size:100000 in
      let path = "/tmp/prop_flush_" ^ string_of_int (Random.int 1000000) ^ ".sst" in
      
      (* Add all entries *)
      let rec add_all ps =
        match ps with
        | [] -> true
        | (entity_id, value) :: rest ->
            let key = make_test_key entity_id in
            match Memtable.add mt ~key ~value with
            | Error _ -> false
            | Ok () -> add_all rest
      in
      
      if not (add_all pairs) then false
      else
        match Memtable.flush_to_sstable mt ~path with
        | Error _ -> false
        | Ok count ->
            if count != List.length pairs then false
            else
              match SSTable.open_read ~path with
              | Error _ -> false
              | Ok reader ->
                  (* Verify all keys *)
                  let all_match = List.for_all (fun (entity_id, value) ->
                    let key = make_test_key entity_id in
                    match SSTable.get reader ~key with
                    | None -> false
                    | Some retrieved -> Bytes.compare retrieved value = 0
                  ) pairs in
                  
                  SSTable.close reader;
                  let _ = Fs.remove_file (Path.v path) in
                  all_match
    )

(** Property: Size accounting is accurate *)
let prop_size_accounting =
  property "Size accounting matches sum of entry sizes"
    Arbitrary.(list (pair arb_key arb_value))
    (fun pairs ->
      let pairs = List.take 10 pairs in
      let mt = Memtable.create ~max_size:100000 in
      
      (* Track actual entries (last write wins) *)
      let entries_map = Collections.HashMap.create () in
      
      let rec add_all ps =
        match ps with
        | [] -> true
        | (entity_id, value) :: rest ->
            let key = make_test_key entity_id in
            
            match Memtable.add mt ~key ~value with
            | Error _ -> assume_fail ()
            | Ok () ->
                (* Track in map (overwrites if exists) *)
                Collections.HashMap.insert entries_map key value;
                add_all rest
      in
      
      if not (add_all pairs) then false
      else
        (* Calculate expected size from final map *)
        let expected_size = Collections.HashMap.fold (fun _key value acc ->
          acc + 41 + Bytes.length value
        ) entries_map 0 in
        
        Memtable.size_bytes mt = expected_size
    )

(** Property: Queries are idempotent *)
let prop_idempotent_queries =
  property "Queries are idempotent"
    Arbitrary.(pair arb_key arb_value)
    (fun (entity_id, value) ->
      let mt = Memtable.create ~max_size:10000 in
      let key = make_test_key entity_id in
      
      match Memtable.add mt ~key ~value with
      | Error _ -> assume_fail ()
      | Ok () ->
          let result1 = Memtable.get mt ~key in
          let result2 = Memtable.get mt ~key in
          
          match (result1, result2) with
          | (Some v1, Some v2) -> Bytes.compare v1 v2 = 0
          | (None, None) -> true
          | _ -> false
    )

(** Property: Last write wins *)
let prop_overwrite_semantics =
  property "Last write wins for duplicate keys"
    Arbitrary.(triple arb_key arb_value arb_value)
    (fun (entity_id, value1, value2) ->
      let mt = Memtable.create ~max_size:10000 in
      let key = make_test_key entity_id in
      
      match Memtable.add mt ~key ~value:value1 with
      | Error _ -> assume_fail ()
      | Ok () ->
          match Memtable.add mt ~key ~value:value2 with
          | Error _ -> assume_fail ()
          | Ok () ->
              match Memtable.get mt ~key with
              | None -> false
              | Some retrieved -> Bytes.compare retrieved value2 = 0
    )

(** {1 Test Suite} *)

let tests =
  [
    (* Unit Tests *)
    Test.case "Create empty memtable" test_create_empty;
    Test.case "Add single entry" test_add_single;
    Test.case "Add multiple entries" test_add_multiple;
    Test.case "Get existing key" test_get_existing;
    Test.case "Get non-existent key" test_get_nonexistent;
    Test.case "Size limit enforced" test_size_limit;
    Test.case "Iteration in sorted order" test_iteration_sorted;
    Test.case "Flush to SSTable" test_flush_to_sstable;
    Test.case "Clear memtable" test_clear;
    Test.case "Overwrite key (last write wins)" test_overwrite;
    
    (* Property Tests *)
    prop_sorted_invariant;
    prop_query_correctness;
    prop_flush_preserves_data;
    prop_size_accounting;
    prop_idempotent_queries;
    prop_overwrite_semantics;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Test.Cli.main ~name:"poneglyph/lsm/memtable" ~tests ~args)
    ~args:Env.args ()
