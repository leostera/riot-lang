open Std
open Poneglyph.Storage.Lsm

module Bytes = Kernel.IO.Bytes

let () = Random.init 42

let setup_test_dir () =
  let test_dir = "/tmp/engine_test_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

(* Helper to create test keys/values *)
let make_key entity_id_int attr_id_int value_int =
  (* Create an EAVT key with simple int64 values *)
  let key : Key.eavt_key = {
    entity_id = Int64.of_int entity_id_int;
    attr_id = Int64.of_int attr_id_int;
    value_kind = Encoding.VK_Int;
    value_repr = Int64.of_int value_int;
    tx_id = 1L;
    fact_id = 1L;
  } in
  Key.encode_eavt key

let make_value str =
  let bytes = Bytes.of_string str in
  bytes

(* ============================= Unit Tests ============================= *)

let test_open_new_engine () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;  (* 1MB *)
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_open_new_engine - " ^ err);
      exit 1
  | Ok engine -> (
      match Engine.close engine with
      | Error err ->
          cleanup_test_dir dir;
          println ("FAIL: test_open_new_engine - close: " ^ err);
          exit 1
      | Ok () ->
          cleanup_test_dir dir;
          println "PASS: test_open_new_engine")

let test_put_and_get () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_put_and_get - " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 1 1 100
      in
      let value = make_value "Alice" in

      match Engine.put engine ~key ~value with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_put_and_get - put: " ^ err);
          exit 1
      | Ok () -> (
          match Engine.get engine ~key with
          | None ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println "FAIL: test_put_and_get - key not found";
              exit 1
          | Some retrieved ->
              if not (Bytes.equal retrieved value) then (
                ignore (Engine.close engine);
                cleanup_test_dir dir;
                println "FAIL: test_put_and_get - value mismatch";
                exit 1)
              else (
                ignore (Engine.close engine);
                cleanup_test_dir dir;
                println "PASS: test_put_and_get")))

let test_get_missing_key () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_get_missing_key - " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 99 99 999
      in

      match Engine.get engine ~key with
      | Some _ ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println "FAIL: test_get_missing_key - found unexpected key";
          exit 1
      | None ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println "PASS: test_get_missing_key")

let test_delete () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_delete - " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 1 1 100
      in
      let value = make_value "Alice" in

      (* Put then delete *)
      match Engine.put engine ~key ~value with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_delete - put: " ^ err);
          exit 1
      | Ok () -> (
          match Engine.delete engine ~key with
          | Error err ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println ("FAIL: test_delete - delete: " ^ err);
              exit 1
          | Ok () -> (
              match Engine.get engine ~key with
              | Some _ ->
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "FAIL: test_delete - key still exists";
                  exit 1
              | None ->
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "PASS: test_delete")))

let test_manual_flush () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_manual_flush - " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 1 1 100
      in
      let value = make_value "Alice" in

      match Engine.put engine ~key ~value with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_manual_flush - put: " ^ err);
          exit 1
      | Ok () -> (
          (* Manual flush *)
          match Engine.flush engine with
          | Error err ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println ("FAIL: test_manual_flush - flush: " ^ err);
              exit 1
          | Ok () -> (
              (* Verify data still accessible *)
              match Engine.get engine ~key with
              | None ->
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "FAIL: test_manual_flush - key not found after flush";
                  exit 1
              | Some retrieved ->
                  if not (Bytes.equal retrieved value) then (
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "FAIL: test_manual_flush - value mismatch";
                    exit 1)
                  else (
                    let stats = Engine.stats engine in
                    if stats.sstable_count != 1 then (
                      ignore (Engine.close engine);
                      cleanup_test_dir dir;
                      println "FAIL: test_manual_flush - expected 1 SSTable";
                      exit 1)
                    else (
                      ignore (Engine.close engine);
                      cleanup_test_dir dir;
                      println "PASS: test_manual_flush")))))

let test_overwrite () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_overwrite - " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 1 1 100
      in
      let value1 = make_value "Alice" in
      let value2 = make_value "Bob" in

      (* Put first value *)
      match Engine.put engine ~key ~value:value1 with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_overwrite - put1: " ^ err);
          exit 1
      | Ok () -> (
          (* Overwrite with second value *)
          match Engine.put engine ~key ~value:value2 with
          | Error err ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println ("FAIL: test_overwrite - put2: " ^ err);
              exit 1
          | Ok () -> (
              (* Should get second value *)
              match Engine.get engine ~key with
              | None ->
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "FAIL: test_overwrite - key not found";
                  exit 1
              | Some retrieved ->
                  if not (Bytes.equal retrieved value2) then (
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "FAIL: test_overwrite - wrong value (should be Bob)";
                    exit 1)
                  else (
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "PASS: test_overwrite"))))

let test_close_and_reopen () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  (* Open, write, close *)
  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_close_and_reopen - open1: " ^ err);
      exit 1
  | Ok engine -> (
      let key =
        make_key 1 1 100
      in
      let value = make_value "Alice" in

      match Engine.put engine ~key ~value with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_close_and_reopen - put: " ^ err);
          exit 1
      | Ok () -> (
          match Engine.close engine with
          | Error err ->
              cleanup_test_dir dir;
              println ("FAIL: test_close_and_reopen - close1: " ^ err);
              exit 1
          | Ok () -> (
              (* Reopen and verify data persisted *)
              match Engine.open_engine config with
              | Error err ->
                  cleanup_test_dir dir;
                  println ("FAIL: test_close_and_reopen - open2: " ^ err);
                  exit 1
              | Ok engine2 -> (
                  match Engine.get engine2 ~key with
                  | None ->
                      ignore (Engine.close engine2);
                      cleanup_test_dir dir;
                      println "FAIL: test_close_and_reopen - key not found after reopen";
                      exit 1
                  | Some retrieved ->
                      if not (Bytes.equal retrieved value) then (
                        ignore (Engine.close engine2);
                        cleanup_test_dir dir;
                        println "FAIL: test_close_and_reopen - value mismatch";
                        exit 1)
                      else (
                        ignore (Engine.close engine2);
                        cleanup_test_dir dir;
                        println "PASS: test_close_and_reopen")))))

let test_stats () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_stats - " ^ err);
      exit 1
  | Ok engine -> (
      let initial_stats = Engine.stats engine in
      if initial_stats.memtable_size != 0 || initial_stats.sstable_count != 0 then (
        ignore (Engine.close engine);
        cleanup_test_dir dir;
        println "FAIL: test_stats - initial stats wrong";
        exit 1)
      else (
        let key =
          make_key 1 1 100
        in
        let value = make_value "Alice" in

        match Engine.put engine ~key ~value with
        | Error err ->
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println ("FAIL: test_stats - put: " ^ err);
            exit 1
        | Ok () -> (
            let after_put_stats = Engine.stats engine in
            if after_put_stats.memtable_size = 0 then (
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println "FAIL: test_stats - memtable should have data";
              exit 1)
            else (
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println "PASS: test_stats"))))

(* ============================= Batch Tests ============================= *)

let test_write_batch_simple () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_write_batch_simple - " ^ err);
      exit 1
  | Ok engine -> (
      let key1 = make_key 1 1 100 in
      let key2 = make_key 2 2 200 in
      let key3 = make_key 3 3 300 in
      let value1 = make_value "Alice" in
      let value2 = make_value "Bob" in
      let value3 = make_value "Carol" in
      
      let ops = [
        `Put (key1, value1);
        `Put (key2, value2);
        `Put (key3, value3);
      ] in
      
      match Engine.write_batch engine ops with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_write_batch_simple - write_batch: " ^ err);
          exit 1
      | Ok () -> (
          (* Verify all keys written *)
          match Engine.get engine ~key:key1 with
          | None ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println "FAIL: test_write_batch_simple - key1 not found";
              exit 1
          | Some v1 -> (
              if not (Bytes.equal v1 value1) then (
                ignore (Engine.close engine);
                cleanup_test_dir dir;
                println "FAIL: test_write_batch_simple - key1 value mismatch";
                exit 1)
              else
                match Engine.get engine ~key:key2 with
                | None ->
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "FAIL: test_write_batch_simple - key2 not found";
                    exit 1
                | Some v2 -> (
                    if not (Bytes.equal v2 value2) then (
                      ignore (Engine.close engine);
                      cleanup_test_dir dir;
                      println "FAIL: test_write_batch_simple - key2 value mismatch";
                      exit 1)
                    else
                      match Engine.get engine ~key:key3 with
                      | None ->
                          ignore (Engine.close engine);
                          cleanup_test_dir dir;
                          println "FAIL: test_write_batch_simple - key3 not found";
                          exit 1
                      | Some v3 -> (
                          if not (Bytes.equal v3 value3) then (
                            ignore (Engine.close engine);
                            cleanup_test_dir dir;
                            println "FAIL: test_write_batch_simple - key3 value mismatch";
                            exit 1)
                          else (
                            ignore (Engine.close engine);
                            cleanup_test_dir dir;
                            println "PASS: test_write_batch_simple"))))))

let test_write_batch_mixed () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_write_batch_mixed - " ^ err);
      exit 1
  | Ok engine -> (
      let key1 = make_key 1 1 100 in
      let key2 = make_key 2 2 200 in
      let key3 = make_key 3 3 300 in
      let value1 = make_value "Alice" in
      let value2 = make_value "Bob" in
      
      (* Pre-populate key3 *)
      match Engine.put engine ~key:key3 ~value:value2 with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_write_batch_mixed - pre-populate: " ^ err);
          exit 1
      | Ok () -> (
          (* Batch with mixed Put and Delete *)
          let ops = [
            `Put (key1, value1);
            `Put (key2, value2);
            `Delete key3;  (* Delete the pre-populated key *)
          ] in
          
          match Engine.write_batch engine ops with
          | Error err ->
              ignore (Engine.close engine);
              cleanup_test_dir dir;
              println ("FAIL: test_write_batch_mixed - write_batch: " ^ err);
              exit 1
          | Ok () -> (
              (* Verify puts succeeded *)
              match Engine.get engine ~key:key1 with
              | None ->
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "FAIL: test_write_batch_mixed - key1 not found";
                  exit 1
              | Some v1 -> (
                  if not (Bytes.equal v1 value1) then (
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "FAIL: test_write_batch_mixed - key1 value mismatch";
                    exit 1)
                  else
                    (* Verify delete succeeded *)
                    match Engine.get engine ~key:key3 with
                    | Some _ ->
                        ignore (Engine.close engine);
                        cleanup_test_dir dir;
                        println "FAIL: test_write_batch_mixed - key3 should be deleted";
                        exit 1
                    | None ->
                        ignore (Engine.close engine);
                        cleanup_test_dir dir;
                        println "PASS: test_write_batch_mixed"))))

let test_write_batch_recovery () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  (* Open engine, write batch, close *)
  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_write_batch_recovery - open1: " ^ err);
      exit 1
  | Ok engine -> (
      let key1 = make_key 1 1 100 in
      let key2 = make_key 2 2 200 in
      let value1 = make_value "Alice" in
      let value2 = make_value "Bob" in
      
      let ops = [
        `Put (key1, value1);
        `Put (key2, value2);
      ] in
      
      match Engine.write_batch engine ops with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_write_batch_recovery - write_batch: " ^ err);
          exit 1
      | Ok () -> (
          match Engine.close engine with
          | Error err ->
              cleanup_test_dir dir;
              println ("FAIL: test_write_batch_recovery - close1: " ^ err);
              exit 1
          | Ok () -> (
              (* Reopen and verify batch was recovered *)
              match Engine.open_engine config with
              | Error err ->
                  cleanup_test_dir dir;
                  println ("FAIL: test_write_batch_recovery - open2: " ^ err);
                  exit 1
              | Ok engine2 -> (
                  match Engine.get engine2 ~key:key1 with
                  | None ->
                      ignore (Engine.close engine2);
                      cleanup_test_dir dir;
                      println "FAIL: test_write_batch_recovery - key1 not recovered";
                      exit 1
                  | Some v1 -> (
                      if not (Bytes.equal v1 value1) then (
                        ignore (Engine.close engine2);
                        cleanup_test_dir dir;
                        println "FAIL: test_write_batch_recovery - key1 value mismatch";
                        exit 1)
                      else
                        match Engine.get engine2 ~key:key2 with
                        | None ->
                            ignore (Engine.close engine2);
                            cleanup_test_dir dir;
                            println "FAIL: test_write_batch_recovery - key2 not recovered";
                            exit 1
                        | Some v2 -> (
                            if not (Bytes.equal v2 value2) then (
                              ignore (Engine.close engine2);
                              cleanup_test_dir dir;
                              println "FAIL: test_write_batch_recovery - key2 value mismatch";
                              exit 1)
                            else (
                              ignore (Engine.close engine2);
                              cleanup_test_dir dir;
                              println "PASS: test_write_batch_recovery")))))))

(* ============================= Integration Tests ============================= *)

let test_write_read_many () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 1024 * 1024;
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_write_read_many - " ^ err);
      exit 1
  | Ok engine -> (
      (* Write 100 keys *)
      let rec write_keys i =
        if i >= 100 then Ok ()
        else
          let key = make_key i i (i * 100) in
          let value = make_value ("value" ^ string_of_int i) in
          match Engine.put engine ~key ~value with
          | Error e -> Error e
          | Ok () -> write_keys (i + 1)
      in

      match write_keys 0 with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_write_read_many - write: " ^ err);
          exit 1
      | Ok () -> (
          (* Read all 100 keys *)
          let rec read_keys i =
            if i >= 100 then true
            else
          let key = make_key i i (i * 100) in
              let expected_value = make_value ("value" ^ string_of_int i) in
              match Engine.get engine ~key with
              | None -> false
              | Some value ->
                  if Bytes.equal value expected_value then read_keys (i + 1)
                  else false
          in

          if not (read_keys 0) then (
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println "FAIL: test_write_read_many - read verification failed";
            exit 1)
          else (
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println "PASS: test_write_read_many")))

let test_needs_compaction () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 100;  (* Very small to force flushes *)
      compaction_threshold = 3;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_needs_compaction - " ^ err);
      exit 1
  | Ok engine -> (
      (* Write keys to trigger multiple flushes *)
      let rec write_and_flush i =
        if i >= 5 then Ok ()
        else
          let key = make_key i i (i * 100) in
          let value = make_value ("value" ^ string_of_int i) in
          match Engine.put engine ~key ~value with
          | Error e -> Error e
          | Ok () -> (
              match Engine.flush engine with
              | Error e -> Error e
              | Ok () -> write_and_flush (i + 1))
      in

      match write_and_flush 0 with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_needs_compaction - write: " ^ err);
          exit 1
      | Ok () ->
          if not (Engine.needs_compaction engine) then (
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println "FAIL: test_needs_compaction - should need compaction";
            exit 1)
          else (
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println "PASS: test_needs_compaction"))

(* Test snapshot isolation: concurrent reads during compaction *)
let test_snapshot_isolation () =
  let dir = setup_test_dir () in
  let config =
    {
      Engine.data_dir = dir;
      max_memtable_size = 100;  (* Tiny to force many flushes *)
      compaction_threshold = 4;
    }
  in

  match Engine.open_engine config with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_snapshot_isolation - " ^ err);
      exit 1
  | Ok engine -> (
      (* Write and flush multiple times to create SSTables *)
      let rec write_sstables i =
        if i >= 5 then Ok ()
        else
          let key = make_key i 1 100 in
          let value = make_value ("snapshot_test_" ^ string_of_int i) in
          match Engine.put engine ~key ~value with
          | Error e -> Error e
          | Ok () -> (
              match Engine.flush engine with
              | Error e -> Error e
              | Ok () -> write_sstables (i + 1))
      in

      match write_sstables 0 with
      | Error err ->
          ignore (Engine.close engine);
          cleanup_test_dir dir;
          println ("FAIL: test_snapshot_isolation - write: " ^ err);
          exit 1
      | Ok () -> (
          (* Verify we have enough SSTables for compaction *)
          let stats = Engine.stats engine in
          if stats.sstable_count < 4 then (
            ignore (Engine.close engine);
            cleanup_test_dir dir;
            println ("FAIL: test_snapshot_isolation - only " ^ 
                     string_of_int stats.sstable_count ^ " SSTables, need >= 4");
            exit 1
          ) else (
            (* Start TWO scans (both acquire manifest snapshots) *)
            let prefix = Bytes.create 0 in
            let iter1 = Engine.scan_prefix engine ~prefix in
            let iter2 = Engine.scan_prefix engine ~prefix in
            
            (* Consume first result from iter1 *)
            let _ = Iter.MutIterator.next iter1 in
            
            (* Now trigger compaction while iterators are still active *)
            match Engine.compact_one_tier engine ~tier:0 ~threshold:4 () with
            | Error err ->
                ignore (Engine.close engine);
                cleanup_test_dir dir;
                println ("FAIL: test_snapshot_isolation - compact: " ^ err);
                exit 1
            | Ok compacted -> (
                if not compacted then (
                  ignore (Engine.close engine);
                  cleanup_test_dir dir;
                  println "FAIL: test_snapshot_isolation - compaction did not happen";
                  exit 1
                ) else (
                  (* Continue reading from BOTH iterators - should still work! *)
                  let rec consume_all iter count =
                    match Iter.MutIterator.next iter with
                    | None -> count
                    | Some _ -> 
                        if count > 100 then (
                          println "ERROR: Iterator infinite loop detected!";
                          count
                        ) else
                          consume_all iter (count + 1)
                  in
                  
                  let total1 = consume_all iter1 1 in  (* Already consumed 1 *)
                  let total2 = consume_all iter2 0 in
                  
                  (* Both should have read all 5 keys *)
                  if total1 < 5 || total2 < 5 then (
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println ("FAIL: test_snapshot_isolation - iter1=" ^ string_of_int total1 ^
                             " iter2=" ^ string_of_int total2 ^ ", expected >= 5 each");
                    exit 1
                  ) else (
                    (* Both iterators exhausted - snapshots released, old files deleted *)
                    ignore (Engine.close engine);
                    cleanup_test_dir dir;
                    println "PASS: test_snapshot_isolation"
                  )
                )
            )
          )
      )
  )

(* ============================= Main ============================= *)

let () =
  println "\n=== LSM Engine Unit Tests ===\n";
  test_open_new_engine ();
  test_put_and_get ();
  test_get_missing_key ();
  test_delete ();
  test_manual_flush ();
  test_overwrite ();
  test_close_and_reopen ();
  test_stats ();

  println "\n=== LSM Engine Batch Tests ===\n";
  test_write_batch_simple ();
  test_write_batch_mixed ();
  test_write_batch_recovery ();

  println "\n=== LSM Engine Integration Tests ===\n";
  test_write_read_many ();
  test_needs_compaction ();
  
  println "\n=== LSM Engine Snapshot Isolation Tests ===\n";
  test_snapshot_isolation ();

  println "\n=== All LSM Engine Tests Passed! ===\n"
