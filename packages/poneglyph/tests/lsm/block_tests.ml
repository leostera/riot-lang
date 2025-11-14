(** Tests for Block - Data block storage *)

open Std
open Std.Collections
open Std.Sync
open Poneglyph

module Block = Poneglyph.Storage.Lsm.Block
module Key = Poneglyph.Storage.Lsm.Key
module Encoding = Poneglyph.Storage.Lsm.Encoding
module Bytes = Kernel.IO.Bytes

(** Helper: create a test EAVT key with given entity_id *)
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

(** Helper: create test value *)
let make_test_value str =
  Bytes.of_string str

(** Test 1: Create empty block *)
let test_create_empty () =
  let block = Block.create () in
  if not (Block.is_empty block) then Error "New block should be empty"
  else if Block.count block != 0 then Error "Empty block should have count 0"
  else if Block.first_key block != None then Error "Empty block should have no first key"
  else if Block.last_key block != None then Error "Empty block should have no last key"
  else Ok ()

(** Test 2: Add single entry *)
let test_add_single_entry () =
  let block = Block.create () in
  let key = make_test_key 1L in
  let value = make_test_value "test_value" in
  
  match Block.add block ~key ~value with
  | Error e -> Error ("Failed to add entry: " ^ e)
  | Ok block ->
      if Block.is_empty block then Error "Block should not be empty after add"
      else if Block.count block != 1 then Error "Block should have count 1"
      else
        match Block.first_key block with
        | None -> Error "Block should have first key"
        | Some fk ->
            if Bytes.compare fk key != 0 then Error "First key mismatch"
            else
              match Block.last_key block with
              | None -> Error "Block should have last key"
              | Some lk ->
                  if Bytes.compare lk key != 0 then Error "Last key should equal first key"
                  else Ok ()

(** Test 3: Add multiple entries *)
let test_add_multiple_entries () =
  let block = Block.create () in
  
  (* Add 10 entries with increasing entity IDs *)
  let rec add_entries b i =
    if i > 10 then Ok b
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match Block.add b ~key ~value with
      | Error e -> Error e
      | Ok b -> add_entries b (i + 1)
  in
  
  match add_entries block 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok block ->
      if Block.count block != 10 then
        Error ("Expected 10 entries, got " ^ string_of_int (Block.count block))
      else
        let first = make_test_key 1L in
        let last = make_test_key 10L in
        match (Block.first_key block, Block.last_key block) with
        | (Some fk, Some lk) ->
            if Bytes.compare fk first != 0 then Error "First key mismatch"
            else if Bytes.compare lk last != 0 then Error "Last key mismatch"
            else Ok ()
        | _ -> Error "Missing first or last key"

(** Test 4: Reject unsorted keys *)
let test_reject_unsorted_keys () =
  let block = Block.create () in
  
  let key1 = make_test_key 2L in
  let key2 = make_test_key 1L in  (* Lower than key1 *)
  let value = make_test_value "test" in
  
  match Block.add block ~key:key1 ~value with
  | Error e -> Error ("First add failed: " ^ e)
  | Ok block ->
      match Block.add block ~key:key2 ~value with
      | Ok _ -> Error "Should reject out-of-order key"
      | Error _ -> Ok ()  (* Expected to fail *)

(** Test 5: Reject duplicate keys *)
let test_reject_duplicate_keys () =
  let block = Block.create () in
  
  let key = make_test_key 1L in
  let value = make_test_value "test" in
  
  match Block.add block ~key ~value with
  | Error e -> Error ("First add failed: " ^ e)
  | Ok block ->
      match Block.add block ~key ~value with
      | Ok _ -> Error "Should reject duplicate key"
      | Error _ -> Ok ()  (* Expected to fail *)

(** Test 6: Get existing key *)
let test_get_existing_key () =
  let block = Block.create () in
  
  let key = make_test_key 5L in
  let value = make_test_value "found_me" in
  
  match Block.add block ~key ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok block ->
      match Block.get block ~key with
      | None -> Error "Failed to find key"
      | Some retrieved ->
          if Bytes.compare retrieved value != 0 then
            Error "Retrieved value doesn't match"
          else Ok ()

(** Test 7: Get non-existent key *)
let test_get_nonexistent_key () =
  let block = Block.create () in
  
  let key1 = make_test_key 1L in
  let key2 = make_test_key 99L in
  let value = make_test_value "test" in
  
  match Block.add block ~key:key1 ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok block ->
      match Block.get block ~key:key2 with
      | Some _ -> Error "Should not find non-existent key"
      | None -> Ok ()

(** Test 8: Binary search correctness *)
let test_binary_search () =
  let block = Block.create () in
  
  (* Add many entries to test binary search *)
  let rec add_entries b i =
    if i > 100 then Ok b
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match Block.add b ~key ~value with
      | Error e -> Error e
      | Ok b -> add_entries b (i + 1)
  in
  
  match add_entries block 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok block ->
      (* Test retrieving various keys *)
      let test_keys = [1; 25; 50; 75; 100] in
      let rec test_retrieval keys =
        match keys with
        | [] -> Ok ()
        | i :: rest ->
            let key = make_test_key (Int64.of_int i) in
            let expected = make_test_value ("value_" ^ string_of_int i) in
            match Block.get block ~key with
            | None -> Error ("Failed to find key " ^ string_of_int i)
            | Some retrieved ->
                if Bytes.compare retrieved expected != 0 then
                  Error ("Value mismatch for key " ^ string_of_int i)
                else test_retrieval rest
      in
      test_retrieval test_keys

(** Test 9: Iterate over entries *)
let test_iterate () =
  let block = Block.create () in
  
  let rec add_entries b i =
    if i > 5 then Ok b
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match Block.add b ~key ~value with
      | Error e -> Error e
      | Ok b -> add_entries b (i + 1)
  in
  
  match add_entries block 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok block ->
      let count = cell 0 in
      Block.iter block ~f:(fun ~key:_ ~value:_ ->
        Cell.set count (Cell.get count + 1)
      );
      
      if Cell.get count != 5 then
        Error ("Expected 5 iterations, got " ^ string_of_int (Cell.get count))
      else Ok ()

(** Test 10: Fold over entries *)
let test_fold () =
  let block = Block.create () in
  
  let rec add_entries b i =
    if i > 3 then Ok b
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value (string_of_int (i * 10)) in
      match Block.add b ~key ~value with
      | Error e -> Error e
      | Ok b -> add_entries b (i + 1)
  in
  
  match add_entries block 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok block ->
      (* Concatenate all values *)
      let result = Block.fold block ~init:"" ~f:(fun ~acc ~key:_ ~value ->
        acc ^ Bytes.to_string value ^ ","
      ) in
      
      if result != "10,20,30," then
        Error ("Fold result mismatch: " ^ result)
      else Ok ()

(** Test 11: Serialization round-trip empty block *)
let test_serialize_empty () =
  let block = Block.create () in
  let bytes = Block.to_bytes block in
  
  match Block.from_bytes bytes with
  | Error e -> Error ("Deserialization failed: " ^ e)
  | Ok block2 ->
      if Block.count block2 != 0 then Error "Deserialized block should be empty"
      else Ok ()

(** Test 12: Serialization round-trip with data *)
let test_serialize_with_data () =
  let block = Block.create () in
  
  let rec add_entries b i =
    if i > 10 then Ok b
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match Block.add b ~key ~value with
      | Error e -> Error e
      | Ok b -> add_entries b (i + 1)
  in
  
  match add_entries block 1 with
  | Error e -> Error ("Failed to add entries: " ^ e)
  | Ok block ->
      let bytes = Block.to_bytes block in
      
      match Block.from_bytes bytes with
      | Error e -> Error ("Deserialization failed: " ^ e)
      | Ok block2 ->
          if Block.count block2 != 10 then
            Error ("Count mismatch: expected 10, got " ^ string_of_int (Block.count block2))
          else
            (* Verify all keys can be retrieved *)
            let rec verify i =
              if i > 10 then Ok ()
              else
                let key = make_test_key (Int64.of_int i) in
                let expected = make_test_value ("value_" ^ string_of_int i) in
                match Block.get block2 ~key with
                | None -> Error ("Key " ^ string_of_int i ^ " not found after deserialization")
                | Some retrieved ->
                    if Bytes.compare retrieved expected != 0 then
                      Error ("Value mismatch after deserialization for key " ^ string_of_int i)
                    else verify (i + 1)
            in
            verify 1

(** Test 13: Block size limits *)
let test_block_size_limits () =
  let block = Block.create () in
  
  (* Create a large value that will nearly fill the block *)
  (* max_data_size = 16256, key = 41, so use 16256 - 41 - 50 = 16165 to leave very little room *)
  let large_value = Bytes.create 16200 in
  let key = make_test_key 1L in
  
  match Block.add block ~key ~value:large_value with
  | Error _ -> Error "Should be able to add large value"
  | Ok block ->
      (* Try to add another entry - should fail (need 41 + 5 = 46 bytes, but only ~15 remain) *)
      let key2 = make_test_key 2L in
      let value2 = make_test_value "small" in
      
      match Block.add block ~key:key2 ~value:value2 with
      | Ok _ -> Error "Should reject entry that exceeds block size"
      | Error _ -> Ok ()

(** Test 14: Invalid key size *)
let test_invalid_key_size () =
  let block = Block.create () in
  let bad_key = Bytes.create 40 in  (* Wrong size *)
  let value = make_test_value "test" in
  
  match Block.add block ~key:bad_key ~value with
  | Ok _ -> Error "Should reject wrong-sized key"
  | Error _ -> Ok ()

(** Test 15: Corrupted data detection *)
let test_corrupted_data_detection () =
  let block = Block.create () in
  
  let key = make_test_key 1L in
  let value = make_test_value "test_value" in
  
  match Block.add block ~key ~value with
  | Error e -> Error ("Failed to add: " ^ e)
  | Ok block ->
      let bytes = Block.to_bytes block in
      
      (* Corrupt the data by flipping a byte in the middle *)
      let corrupt_pos = Bytes.length bytes / 2 in
      let orig_byte = Bytes.get_uint8 bytes corrupt_pos in
      Bytes.set_uint8 bytes corrupt_pos (orig_byte lxor 0xFF);
      
      match Block.from_bytes bytes with
      | Ok _ -> Error "Should detect corrupted data"
      | Error msg ->
          if msg = "Block checksum mismatch" then Ok ()
          else Error ("Expected checksum error, got: " ^ msg)

let tests =
  Test.
    [
      case "Create empty block" test_create_empty;
      case "Add single entry" test_add_single_entry;
      case "Add multiple entries" test_add_multiple_entries;
      case "Reject unsorted keys" test_reject_unsorted_keys;
      case "Reject duplicate keys" test_reject_duplicate_keys;
      case "Get existing key" test_get_existing_key;
      case "Get non-existent key" test_get_nonexistent_key;
      case "Binary search correctness" test_binary_search;
      case "Iterate over entries" test_iterate;
      case "Fold over entries" test_fold;
      case "Serialize empty block" test_serialize_empty;
      case "Serialize with data" test_serialize_with_data;
      case "Block size limits" test_block_size_limits;
      case "Invalid key size" test_invalid_key_size;
      case "Corrupted data detection" test_corrupted_data_detection;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Test.Cli.main ~name:"poneglyph/lsm/block" ~tests ~args)
    ~args:Env.args ()
