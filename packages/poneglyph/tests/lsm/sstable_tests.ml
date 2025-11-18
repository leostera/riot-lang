(** Tests for SSTable - Sorted String Table storage *)

open Std
open Std.Collections
open Std.Sync
open Poneglyph

module SSTable = Poneglyph.Storage.Lsm.Sstable
module Key = Poneglyph.Storage.Lsm.Key
module Encoding = Poneglyph.Storage.Lsm.Encoding
module Bytes = Kernel.IO.Bytes

(** Test directory for SSTable files *)
let test_dir = "/tmp/poneglyph_sstable_tests"

(** Ensure test directory exists *)
let setup_test_dir () =
  match Fs.metadata (Path.v test_dir) with
  | Error _ -> let _ = Fs.create_dir (Path.v test_dir) in ()
  | Ok _ -> ()

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

(** Helper: get unique test file path *)
let test_counter = cell 0
let make_test_path () =
  setup_test_dir ();
  let n = Cell.get test_counter in
  Cell.set test_counter (n + 1);
  test_dir ^ "/test_" ^ string_of_int n ^ ".sst"

(** Test 1: Create empty SSTable *)
let test_create_empty () =
  let path = make_test_path () in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok builder ->
      match SSTable.finalize builder with
      | Error e -> Error ("Failed to finalize empty SSTable: " ^ e)
      | Ok count ->
          if count != 0 then
            Error ("Empty SSTable should have 0 entries, got " ^ string_of_int count)
          else
            (* Try to read it back *)
            match SSTable.open_read ~path with
            | Error e -> Error ("Failed to open empty SSTable: " ^ e)
            | Ok reader ->
                if SSTable.entry_count reader != 0 then
                  Error "Empty SSTable should have 0 entries after read"
                else if SSTable.block_count reader != 0 then
                  Error "Empty SSTable should have 0 blocks"
                else (
                  SSTable.close reader;
                  Ok ()
                )

(** Test 2: Write and read single entry *)
let test_single_entry () =
  let path = make_test_path () in
  
  let key = make_test_key 1L in
  let value = make_test_value "test_value" in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok builder ->
      match SSTable.add builder ~key ~value with
      | Error e -> Error ("Failed to add entry: " ^ e)
      | Ok builder ->
          match SSTable.finalize builder with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok count ->
              if count != 1 then
                Error ("Expected 1 entry, got " ^ string_of_int count)
              else
                match SSTable.open_read ~path with
                | Error e -> Error ("Failed to open: " ^ e)
                | Ok reader ->
                    match SSTable.get reader ~key with
                    | None -> Error "Failed to find key"
                    | Some retrieved ->
                        SSTable.close reader;
                        if Bytes.compare retrieved value != 0 then
                          Error "Value mismatch"
                        else Ok ()

(** Test 3: Write and read multiple entries *)
let test_multiple_entries () =
  let path = make_test_path () in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok initial_builder ->
      let builder = cell initial_builder in
      
      (* Add 100 entries *)
      let rec add_entries i =
    if i > 100 then Ok ()
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value ("value_" ^ string_of_int i) in
      match SSTable.add (Cell.get builder) ~key ~value with
      | Error e -> Error e
      | Ok b ->
          Cell.set builder b;
          add_entries (i + 1)
      in
      
      match add_entries 1 with
      | Error e -> Error ("Failed to add entries: " ^ e)
      | Ok () ->
          match SSTable.finalize (Cell.get builder) with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok count ->
              if count != 100 then
                Error ("Expected 100 entries, got " ^ string_of_int count)
              else
                match SSTable.open_read ~path with
                | Error e -> Error ("Failed to open: " ^ e)
                | Ok reader ->
                    (* Verify all entries *)
                    let rec verify i =
                      if i > 100 then Ok ()
                      else
                        let key = make_test_key (Int64.of_int i) in
                        let expected = make_test_value ("value_" ^ string_of_int i) in
                        match SSTable.get reader ~key with
                        | None ->
                            SSTable.close reader;
                            Error ("Key " ^ string_of_int i ^ " not found")
                        | Some retrieved ->
                            if Bytes.compare retrieved expected != 0 then (
                              SSTable.close reader;
                              Error ("Value mismatch for key " ^ string_of_int i)
                            ) else
                              verify (i + 1)
                    in
                    match verify 1 with
                    | Error e -> Error e
                    | Ok () ->
                        SSTable.close reader;
                        Ok ()

(** Test 4: Reject out-of-order keys *)
let test_reject_out_of_order () =
  let path = make_test_path () in
  
  let key1 = make_test_key 2L in
  let key2 = make_test_key 1L in  (* Lower than key1 *)
  let value = make_test_value "test" in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok builder ->
      match SSTable.add builder ~key:key1 ~value with
      | Error e -> Error ("First add failed: " ^ e)
      | Ok builder ->
          match SSTable.add builder ~key:key2 ~value with
          | Ok _ -> Error "Should reject out-of-order key"
          | Error _ -> Ok ()  (* Expected *)

(** Test 5: Multiple blocks (large SSTable) *)
let test_multiple_blocks () =
  let path = make_test_path () in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok initial_builder ->
      let builder = cell initial_builder in
      
      (* Add enough entries to fill multiple blocks *)
      (* Block is ~16KB, with key (41 bytes) + value (100 bytes) = 141 bytes per entry *)
      (* So ~116 entries per block. Add 300 to get ~3 blocks *)
      let rec add_entries i =
        if i > 300 then Ok ()
        else
          let key = make_test_key (Int64.of_int i) in
          let value = make_test_value (String.init 100 (fun _ -> 'x')) in
          match SSTable.add (Cell.get builder) ~key ~value with
          | Error e -> Error e
          | Ok b ->
              Cell.set builder b;
              add_entries (i + 1)
      in
      
      match add_entries 1 with
      | Error e -> Error ("Failed to add entries: " ^ e)
      | Ok () ->
          match SSTable.finalize (Cell.get builder) with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok _count ->
              match SSTable.open_read ~path with
              | Error e -> Error ("Failed to open: " ^ e)
              | Ok reader ->
                  let blocks = SSTable.block_count reader in
                  if blocks < 2 then (
                    SSTable.close reader;
                    Error ("Expected multiple blocks, got " ^ string_of_int blocks)
                  ) else
                    (* Verify first, middle, and last keys *)
                    let test_keys = [1; 150; 300] in
                    let rec verify_keys keys =
                      match keys with
                      | [] -> Ok ()
                      | k :: rest ->
                          let key = make_test_key (Int64.of_int k) in
                          match SSTable.get reader ~key with
                          | None ->
                              SSTable.close reader;
                              Error ("Key " ^ string_of_int k ^ " not found across blocks")
                          | Some _ -> verify_keys rest
                    in
                    match verify_keys test_keys with
                    | Error e -> Error e
                    | Ok () ->
                        SSTable.close reader;
                        Ok ()

(** Test 6: Get non-existent key *)
let test_get_nonexistent () =
  let path = make_test_path () in
  
  let key1 = make_test_key 1L in
  let key2 = make_test_key 99L in
  let value = make_test_value "test" in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok builder ->
      match SSTable.add builder ~key:key1 ~value with
      | Error e -> Error ("Failed to add: " ^ e)
      | Ok builder ->
          match SSTable.finalize builder with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok _ ->
              match SSTable.open_read ~path with
              | Error e -> Error ("Failed to open: " ^ e)
              | Ok reader ->
                  match SSTable.get reader ~key:key2 with
                  | Some _ ->
                      SSTable.close reader;
                      Error "Should not find non-existent key"
                  | None ->
                      SSTable.close reader;
                      Ok ()

(** Test 7: Iterate over all entries *)
let test_iterate () =
  let path = make_test_path () in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok initial_builder ->
      let builder = cell initial_builder in
      
      let rec add_entries i =
    if i > 10 then Ok ()
    else
      let key = make_test_key (Int64.of_int i) in
      let value = make_test_value (string_of_int i) in
      match SSTable.add (Cell.get builder) ~key ~value with
      | Error e -> Error e
      | Ok b ->
          Cell.set builder b;
          add_entries (i + 1)
      in
      
      match add_entries 1 with
      | Error e -> Error ("Failed to add: " ^ e)
      | Ok () ->
          match SSTable.finalize (Cell.get builder) with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok _ ->
              match SSTable.open_read ~path with
              | Error e -> Error ("Failed to open: " ^ e)
              | Ok reader ->
                  let count = cell 0 in
                  SSTable.iter reader ~f:(fun ~key:_ ~value:_ ->
                    Cell.set count (Cell.get count + 1)
                  );
                  SSTable.close reader;
                  
                  if Cell.get count != 10 then
                    Error ("Expected 10 iterations, got " ^ string_of_int (Cell.get count))
                  else Ok ()

(** Test 8: First and last key accessors *)
let test_first_last_keys () =
  let path = make_test_path () in
  
  let first_k = make_test_key 1L in
  let last_k = make_test_key 10L in
  let value = make_test_value "test" in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok initial_builder ->
      let builder = cell initial_builder in
      
      let rec add_entries i =
    if i > 10 then Ok ()
    else
      let key = make_test_key (Int64.of_int i) in
      match SSTable.add (Cell.get builder) ~key ~value with
      | Error e -> Error e
      | Ok b ->
          Cell.set builder b;
          add_entries (i + 1)
      in
      
      match add_entries 1 with
      | Error e -> Error ("Failed to add: " ^ e)
      | Ok () ->
          match SSTable.finalize (Cell.get builder) with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok _ ->
              match SSTable.open_read ~path with
              | Error e -> Error ("Failed to open: " ^ e)
              | Ok reader ->
                  let first = SSTable.first_key reader in
                  let last = SSTable.last_key reader in
                  SSTable.close reader;
                  
                  if Bytes.compare first first_k != 0 then
                    Error "First key mismatch"
                  else if Bytes.compare last last_k != 0 then
                    Error "Last key mismatch"
                  else Ok ()

(** Test 9: Range check *)
let test_in_range () =
  let path = make_test_path () in
  
  let value = make_test_value "test" in
  
  match SSTable.create_builder ~path with
  | Error e -> Error ("Failed to create builder: " ^ e)
  | Ok initial_builder ->
      let builder = cell initial_builder in
      
      let rec add_entries i =
        if i > 10 then Ok ()
        else
          let key = make_test_key (Int64.of_int i) in
          match SSTable.add (Cell.get builder) ~key ~value with
          | Error e -> Error e
          | Ok b ->
              Cell.set builder b;
              add_entries (i + 1)
      in
      
      match add_entries 1 with
      | Error e -> Error ("Failed to add: " ^ e)
      | Ok () ->
          match SSTable.finalize (Cell.get builder) with
          | Error e -> Error ("Failed to finalize: " ^ e)
          | Ok _ ->
              match SSTable.open_read ~path with
              | Error e -> Error ("Failed to open: " ^ e)
              | Ok reader ->
                  let key_before = make_test_key 0L in
                  let key_inside = make_test_key 5L in
                  let key_after = make_test_key 99L in
                  
                  let before_in_range = SSTable.in_range reader ~key:key_before in
                  let inside_in_range = SSTable.in_range reader ~key:key_inside in
                  let after_in_range = SSTable.in_range reader ~key:key_after in
                  
                  SSTable.close reader;
                  
                  if before_in_range then
                    Error "Key before range should not be in range"
                  else if not inside_in_range then
                    Error "Key inside range should be in range"
                  else if after_in_range then
                    Error "Key after range should not be in range"
                  else Ok ()

(** Test 10: Open non-existent file *)
let test_open_nonexistent () =
  match SSTable.open_read ~path:"/nonexistent/path.sst" with
  | Ok _ -> Error "Should fail to open non-existent file"
  | Error _ -> Ok ()

let tests =
  Test.
    [
      case "Create empty SSTable" test_create_empty;
      case "Write and read single entry" test_single_entry;
      case "Write and read multiple entries" test_multiple_entries;
      case "Reject out-of-order keys" test_reject_out_of_order;
      case "Multiple blocks" test_multiple_blocks;
      case "Get non-existent key" test_get_nonexistent;
      case "Iterate over entries" test_iterate;
      case "First and last keys" test_first_last_keys;
      case "Range check" test_in_range;
      case "Open non-existent file" test_open_nonexistent;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Test.Cli.main ~name:"poneglyph/lsm/sstable" ~tests ~args)
    ~args:Env.args ()
