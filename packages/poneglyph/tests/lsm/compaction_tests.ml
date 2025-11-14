open Std
open Std.Collections
open Poneglyph.Storage.Lsm

module Bytes = Kernel.IO.Bytes

let () = Random.init 42

let setup_test_dir () =
  let test_dir =
    "/tmp/compaction_test_" ^ string_of_int (Random.int 1000000)
  in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

let make_test_path dir name = dir ^ "/" ^ name ^ ".sst"

(* Helper to create an SSTable with test data *)
let create_test_sstable path entries =
  let builder = Sstable.create_builder ~path in
  let rec add_all builder entries =
    match entries with
    | [] -> Ok builder
    | (key, value) :: rest -> (
        match Sstable.add builder ~key ~value with
        | Error err -> Error err
        | Ok new_builder -> add_all new_builder rest)
  in
  match add_all builder entries with
  | Error err -> Error err
  | Ok final_builder -> (
      match Sstable.finalize final_builder with
      | Error err -> Error err
      | Ok _ -> Ok ())

(* Helper to read all entries from an SSTable *)
let read_sstable path =
  match Sstable.open_read ~path with
  | Error err -> Error err
  | Ok reader ->
      let entries = Vector.create () in
      Sstable.iter reader ~f:(fun ~key ~value ->
          Vector.push entries (key, value));
      Sstable.close reader;
      let iter = Vector.to_mut_iter entries in
      Ok (Iter.MutIterator.to_list iter)

(* Helper to make a key from components *)
let make_key entity_id_int attr_id_int value_int =
  let key : Key.eavt_key = {
    entity_id = Int64.of_int entity_id_int;
    attr_id = Int64.of_int attr_id_int;
    value_kind = Encoding.VK_Int;
    value_repr = Int64.of_int value_int;
    tx_id = 1L;
    fact_id = 1L;
  } in
  Key.encode_eavt key

(* Helper to make an encoded value *)
let make_value str =
  Bytes.of_string str

(* ============================= Unit Tests ============================= *)

let test_merge_two_sstables () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let path2 = make_test_path dir "input2" in
  let output_path = make_test_path dir "output" in

  (* Create first SSTable *)
  let entries1 =
    [
      ( make_key 1 1 100,
        make_value "Alice" );
      ( make_key 2 2 100,
        make_value "30" );
    ]
  in

  (* Create second SSTable *)
  let entries2 =
    [
      ( make_key 3 3 100,
        make_value "NYC" );
      ( make_key 4 4 100,
        make_value "NY" );
    ]
  in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_two_sstables - create1: " ^ err);
      exit 1
  | Ok () -> (
      match create_test_sstable path2 entries2 with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_two_sstables - create2: " ^ err);
          exit 1
      | Ok () -> (
          match
            Compaction.merge_sstables ~inputs:[ path1; path2 ] ~output:output_path
          with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_two_sstables - merge: " ^ err);
              exit 1
          | Ok () -> (
              match read_sstable output_path with
              | Error err ->
                  cleanup_test_dir dir;
                  println
                    ("FAIL: test_merge_two_sstables - read: " ^
                       err);
                  exit 1
              | Ok entries ->
                  if List.length entries != 4 then (
                    cleanup_test_dir dir;
                    println
                      ("FAIL: test_merge_two_sstables - expected 4 entries, \
                          got " ^ string_of_int (List.length entries));
                    exit 1)
                  else (
                    cleanup_test_dir dir;
                    println "PASS: test_merge_two_sstables"))))

let test_merge_preserves_order () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let path2 = make_test_path dir "input2" in
  let output_path = make_test_path dir "output" in

  (* Create SSTables with overlapping ranges *)
  let entries1 =
    [
      ( make_key 1 1 100,
        make_value "Alice" );
      ( make_key 3 3 100,
        make_value "LA" );
    ]
  in

  let entries2 =
    [
      ( make_key 2 2 100,
        make_value "30" );
      ( make_key 4 4 100,
        make_value "CA" );
    ]
  in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_preserves_order - create1: " ^ err);
      exit 1
  | Ok () -> (
      match create_test_sstable path2 entries2 with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_preserves_order - create2: " ^ err);
          exit 1
      | Ok () -> (
          match
            Compaction.merge_sstables ~inputs:[ path1; path2 ] ~output:output_path
          with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_preserves_order - merge: " ^
                   err);
              exit 1
          | Ok () -> (
              match read_sstable output_path with
              | Error err ->
                  cleanup_test_dir dir;
                  println
                    ("FAIL: test_merge_preserves_order - read: " ^
                       err);
                  exit 1
              | Ok entries ->
                  (* Verify sorted order *)
                  let rec is_sorted entries =
                    match entries with
                    | [] | [ _ ] -> true
                    | (k1, _) :: ((k2, _) :: _ as rest) ->
                        if Bytes.compare k1 k2 < 0 then is_sorted rest else false
                  in
                  if not (is_sorted entries) then (
                    cleanup_test_dir dir;
                    println
                      "FAIL: test_merge_preserves_order - not sorted";
                    exit 1)
                  else (
                    cleanup_test_dir dir;
                    println "PASS: test_merge_preserves_order"))))

let test_merge_deduplicates () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let path2 = make_test_path dir "input2" in
  let output_path = make_test_path dir "output" in

  (* Same key in both SSTables *)
  let key = make_key 1 1 100 in

  let entries1 = [ (key, make_value "Alice") ] in
  let entries2 = [ (key, make_value "Bob") ] in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_deduplicates - create1: " ^ err);
      exit 1
  | Ok () -> (
      match create_test_sstable path2 entries2 with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_deduplicates - create2: " ^ err);
          exit 1
      | Ok () -> (
          match
            Compaction.merge_sstables ~inputs:[ path1; path2 ] ~output:output_path
          with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_deduplicates - merge: " ^ err);
              exit 1
          | Ok () -> (
              match read_sstable output_path with
              | Error err ->
                  cleanup_test_dir dir;
                  println
                    ("FAIL: test_merge_deduplicates - read: " ^
                       err);
                  exit 1
              | Ok entries ->
                  if List.length entries != 1 then (
                    cleanup_test_dir dir;
                    println
                      ("FAIL: test_merge_deduplicates - expected 1 entry, got \
                          " ^ string_of_int (List.length entries));
                    exit 1)
                  else
                    (* Verify it's the value from path2 (later input wins) *)
                    let _, value = List.hd entries in
                    let expected_value = make_value "Bob" in
                    if not (Bytes.equal value expected_value) then (
                      cleanup_test_dir dir;
                      println
                        "FAIL: test_merge_deduplicates - wrong value (should be \
                         Bob)";
                      exit 1)
                    else (
                      cleanup_test_dir dir;
                      println "PASS: test_merge_deduplicates"))))

let test_merge_last_wins () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let path2 = make_test_path dir "input2" in
  let path3 = make_test_path dir "input3" in
  let output_path = make_test_path dir "output" in

  let key = make_key 1 1 100 in

  let entries1 = [ (key, make_value "v1") ] in
  let entries2 = [ (key, make_value "v2") ] in
  let entries3 = [ (key, make_value "v3") ] in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_last_wins - create1: " ^ err);
      exit 1
  | Ok () -> (
      match create_test_sstable path2 entries2 with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_last_wins - create2: " ^ err);
          exit 1
      | Ok () -> (
          match create_test_sstable path3 entries3 with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_last_wins - create3: " ^ err);
              exit 1
          | Ok () -> (
              match
                Compaction.merge_sstables
                  ~inputs:[ path1; path2; path3 ]
                  ~output:output_path
              with
              | Error err ->
                  cleanup_test_dir dir;
                  println
                    ("FAIL: test_merge_last_wins - merge: " ^ err);
                  exit 1
              | Ok () -> (
                  match read_sstable output_path with
                  | Error err ->
                      cleanup_test_dir dir;
                      println
                        ("FAIL: test_merge_last_wins - read: " ^
                           err);
                      exit 1
                  | Ok entries ->
                      let _, value = List.hd entries in
                      let expected_value = make_value "v3" in
                      if not (Bytes.equal value expected_value) then (
                        cleanup_test_dir dir;
                        println
                          "FAIL: test_merge_last_wins - should be v3 (last)";
                        exit 1)
                      else (
                        cleanup_test_dir dir;
                        println "PASS: test_merge_last_wins")))))

let test_merge_single_sstable () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let output_path = make_test_path dir "output" in

  let entries1 =
    [
      ( make_key 1 1 100,
        make_value "Alice" );
    ]
  in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_single_sstable - create: " ^ err);
      exit 1
  | Ok () -> (
      match Compaction.merge_sstables ~inputs:[ path1 ] ~output:output_path with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_single_sstable - merge: " ^ err);
          exit 1
      | Ok () -> (
          match read_sstable output_path with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_single_sstable - read: " ^ err);
              exit 1
          | Ok entries ->
              if List.length entries != 1 then (
                cleanup_test_dir dir;
                println "FAIL: test_merge_single_sstable - wrong count";
                exit 1)
              else (
                cleanup_test_dir dir;
                println "PASS: test_merge_single_sstable")))

let test_compact_deletes_inputs () =
  let dir = setup_test_dir () in
  let path1 = make_test_path dir "input1" in
  let path2 = make_test_path dir "input2" in
  let output_path = make_test_path dir "output" in

  let entries1 =
    [
      ( make_key 1 1 100,
        make_value "Alice" );
    ]
  in

  let entries2 =
    [
      ( make_key 2 2 100,
        make_value "30" );
    ]
  in

  match create_test_sstable path1 entries1 with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_compact_deletes_inputs - create1: " ^ err);
      exit 1
  | Ok () -> (
      match create_test_sstable path2 entries2 with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_compact_deletes_inputs - create2: " ^ err);
          exit 1
      | Ok () -> (
          match
            Compaction.compact ~inputs:[ path1; path2 ] ~output:output_path
              ~delete_inputs:true
          with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_compact_deletes_inputs - compact: " ^
                   err);
              exit 1
          | Ok () ->
              (* Check that inputs were deleted *)
              let exists1 = match Fs.exists (Path.v path1) with Ok b -> b | Error _ -> false in
              let exists2 = match Fs.exists (Path.v path2) with Ok b -> b | Error _ -> false in
              if exists1 || exists2 then (
                cleanup_test_dir dir;
                println
                  "FAIL: test_compact_deletes_inputs - inputs not deleted";
                exit 1)
              else (
                cleanup_test_dir dir;
                println "PASS: test_compact_deletes_inputs")))

let test_merge_many_sstables () =
  let dir = setup_test_dir () in

  (* Create 5 SSTables *)
  let paths =
    [
      make_test_path dir "input1";
      make_test_path dir "input2";
      make_test_path dir "input3";
      make_test_path dir "input4";
      make_test_path dir "input5";
    ]
  in

  let output_path = make_test_path dir "output" in

  (* Each SSTable gets one unique entry *)
  let all_entries =
    List.mapi
      (fun i path ->
        let key =
          make_key i i (i * 100)
        in
        let value = make_value ("value" ^ string_of_int i) in
        (path, [ (key, value) ]))
      paths
  in

  (* Create all SSTables *)
  let rec create_all entries =
    match entries with
    | [] -> Ok ()
    | (path, ents) :: rest -> (
        match create_test_sstable path ents with
        | Error err -> Error err
        | Ok () -> create_all rest)
  in

  match create_all all_entries with
  | Error err ->
      cleanup_test_dir dir;
      println
        ("FAIL: test_merge_many_sstables - create: " ^ err);
      exit 1
  | Ok () -> (
      match Compaction.merge_sstables ~inputs:paths ~output:output_path with
      | Error err ->
          cleanup_test_dir dir;
          println
            ("FAIL: test_merge_many_sstables - merge: " ^ err);
          exit 1
      | Ok () -> (
          match read_sstable output_path with
          | Error err ->
              cleanup_test_dir dir;
              println
                ("FAIL: test_merge_many_sstables - read: " ^ err);
              exit 1
          | Ok entries ->
              if List.length entries != 5 then (
                cleanup_test_dir dir;
                println
                  ("FAIL: test_merge_many_sstables - expected 5 entries, got \
                      " ^ string_of_int (List.length entries));
                exit 1)
              else (
                cleanup_test_dir dir;
                println "PASS: test_merge_many_sstables")))

(* ============================= Main ============================= *)

let () =
  println "\n=== Compaction Unit Tests ===\n";
  test_merge_two_sstables ();
  test_merge_preserves_order ();
  test_merge_deduplicates ();
  test_merge_last_wins ();
  test_merge_single_sstable ();
  test_compact_deletes_inputs ();
  test_merge_many_sstables ();

  println "\n=== All Compaction Tests Passed! ===\n"
