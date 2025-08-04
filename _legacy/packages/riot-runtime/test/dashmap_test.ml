open Riot_runtime

(* Test module for concurrent hashmap (Dashmap) *)

module StringMap = Dashmap.Make(struct
  type key = string
  let hash = Hashtbl.hash
  let equal = String.equal
end)

module IntMap = Dashmap.Make(struct
  type key = int
  let hash = Hashtbl.hash
  let equal = Int.equal
end)

let test_empty_map () =
  let map = StringMap.create () in
  assert (StringMap.is_empty map);
  assert (StringMap.get map "key" = None);
  assert (not (StringMap.has_key map "key"));
  Printf.printf "✓ Empty map test passed\n"

let test_basic_operations () =
  let map = StringMap.create () in
  
  (* Insert and retrieve *)
  StringMap.insert map "key1" "value1";
  assert (not (StringMap.is_empty map));
  assert (StringMap.has_key map "key1");
  assert (StringMap.get map "key1" = Some "value1");
  assert (StringMap.get map "nonexistent" = None);
  
  (* Replace *)
  StringMap.replace map "key1" "new_value1";
  assert (StringMap.get map "key1" = Some "new_value1");
  
  (* Remove *)
  StringMap.remove map "key1";
  assert (not (StringMap.has_key map "key1"));
  assert (StringMap.get map "key1" = None);
  assert (StringMap.is_empty map);
  
  Printf.printf "✓ Basic operations test passed\n"

let test_multiple_values_same_key () =
  let map = StringMap.create () in
  
  (* Insert multiple values for same key *)
  StringMap.insert map "key" "value1";
  StringMap.insert map "key" "value2";
  StringMap.insert map "key" "value3";
  
  let all_values = StringMap.get_all map "key" in
  assert (List.length all_values = 3);
  assert (List.mem "value1" all_values);
  assert (List.mem "value2" all_values);
  assert (List.mem "value3" all_values);
  
  Printf.printf "✓ Multiple values same key test passed\n"

let test_find_operations () =
  let map = StringMap.create () in
  
  StringMap.insert map "user1" "alice";
  StringMap.insert map "user2" "bob";
  StringMap.insert map "admin1" "charlie";
  StringMap.insert map "admin2" "diana";
  
  (* Find by predicate *)
  let find_admin = StringMap.find_by map (fun (k, _v) -> String.starts_with ~prefix:"admin" k) in
  assert (find_admin <> None);
  
  let all_admins = StringMap.find_all_by map (fun (k, _v) -> String.starts_with ~prefix:"admin" k) in
  assert (List.length all_admins = 2);
  
  Printf.printf "✓ Find operations test passed\n"

let test_remove_operations () =
  let map = StringMap.create () in
  
  StringMap.insert map "keep1" "value1";
  StringMap.insert map "remove1" "value2";
  StringMap.insert map "keep2" "value3";
  StringMap.insert map "remove2" "value4";
  
  (* Remove by predicate *)
  StringMap.remove_by map (fun (k, _v) -> String.starts_with ~prefix:"remove" k);
  
  assert (StringMap.has_key map "keep1");
  assert (StringMap.has_key map "keep2");
  assert (not (StringMap.has_key map "remove1"));
  assert (not (StringMap.has_key map "remove2"));
  
  (* Remove multiple keys *)
  StringMap.remove_all map ["keep1"; "keep2"; "nonexistent"];
  assert (StringMap.is_empty map);
  
  Printf.printf "✓ Remove operations test passed\n"

let test_iteration () =
  let map = IntMap.create () in
  let expected_pairs = [(1, "one"); (2, "two"); (3, "three")] in
  
  List.iter (fun (k, v) -> IntMap.insert map k v) expected_pairs;
  
  let collected = ref [] in
  IntMap.iter map (fun (k, v) -> collected := (k, v) :: !collected);
  
  let sorted_collected = List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2) !collected in
  assert (sorted_collected = expected_pairs);
  
  Printf.printf "✓ Iteration test passed\n"

(* Concurrent testing using Domains *)
let test_concurrent_insertions () =
  let map = IntMap.create () in
  let num_domains = 4 in
  let ops_per_domain = 1000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      for i = 0 to ops_per_domain - 1 do
        let key = domain_id * ops_per_domain + i in
        IntMap.insert map key (Printf.sprintf "value_%d_%d" domain_id i)
      done
    )
  ) in
  
  Array.iter Domain.join domains;
  
  (* Verify all insertions succeeded *)
  let total_expected = num_domains * ops_per_domain in
  let collected = ref [] in
  IntMap.iter map (fun pair -> collected := pair :: !collected);
  assert (List.length !collected = total_expected);
  
  Printf.printf "✓ Concurrent insertions test passed (%d items)\n" total_expected

let test_concurrent_mixed_operations () =
  let map = IntMap.create () in
  let num_domains = 6 in
  let ops_per_domain = 500 in
  
  (* Pre-populate with some data *)
  for i = 0 to 1000 do
    IntMap.insert map i (Printf.sprintf "initial_%d" i)
  done;
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      let local_ops = ref 0 in
      for i = 0 to ops_per_domain - 1 do
        let key = Random.int 2000 in
        match Random.int 4 with
        | 0 -> (* Insert *)
            IntMap.insert map key (Printf.sprintf "new_%d_%d" domain_id i);
            incr local_ops
        | 1 -> (* Get *)
            ignore (IntMap.get map key : string option)
        | 2 -> (* Replace *)
            IntMap.replace map key (Printf.sprintf "replaced_%d_%d" domain_id i)
        | 3 -> (* Remove *)
            IntMap.remove map key;
            incr local_ops
        | _ -> assert false
      done;
      !local_ops
    )
  ) in
  
  let operation_counts = Array.map Domain.join domains in
  let total_ops = Array.fold_left (+) 0 operation_counts in
  
  Printf.printf "✓ Concurrent mixed operations test passed (%d operations)\n" total_ops

let test_concurrent_readers_writers () =
  let map = IntMap.create () in
  let num_items = 1000 in
  
  (* Initialize map *)
  for i = 0 to num_items - 1 do
    IntMap.insert map i (Printf.sprintf "value_%d" i)
  done;
  
  let num_readers = 4 in
  let num_writers = 2 in
  let reads_per_reader = 2000 in
  let writes_per_writer = 500 in
  
  (* Reader domains *)
  let readers = Array.init num_readers (fun reader_id ->
    Domain.spawn (fun () ->
      let successful_reads = ref 0 in
      for i = 0 to reads_per_reader - 1 do
        let key = Random.int num_items in
        match IntMap.get map key with
        | Some _ -> incr successful_reads
        | None -> ()
      done;
      !successful_reads
    )
  ) in
  
  (* Writer domains *)
  let writers = Array.init num_writers (fun writer_id ->
    Domain.spawn (fun () ->
      for i = 0 to writes_per_writer - 1 do
        let key = Random.int num_items in
        if Random.bool () then
          IntMap.replace map key (Printf.sprintf "updated_%d_%d" writer_id i)
        else
          IntMap.remove map key
      done
    )
  ) in
  
  let read_counts = Array.map Domain.join readers in
  Array.iter Domain.join writers;
  
  let total_reads = Array.fold_left (+) 0 read_counts in
  Printf.printf "✓ Concurrent readers/writers test passed (%d successful reads)\n" total_reads

let test_stress_operations () =
  let map = StringMap.create () in
  let num_domains = 8 in
  let ops_per_domain = 1000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      for i = 0 to ops_per_domain - 1 do
        let key = Printf.sprintf "key_%d_%d" domain_id (Random.int 100) in
        let value = Printf.sprintf "value_%d_%d" domain_id i in
        
        match Random.int 6 with
        | 0 -> StringMap.insert map key value
        | 1 -> ignore (StringMap.get map key : string option)
        | 2 -> StringMap.replace map key value
        | 3 -> StringMap.remove map key
        | 4 -> ignore (StringMap.has_key map key : bool)
        | 5 -> ignore (StringMap.get_all map key : string list)
        | _ -> assert false
      done
    )
  ) in
  
  Array.iter Domain.join domains;
  
  (* Verify map is still in valid state *)
  let final_size = ref 0 in
  StringMap.iter map (fun _ -> incr final_size);
  
  Printf.printf "✓ Stress operations test passed (final size: %d)\n" !final_size

(* Property: Thread safety invariants *)
let test_property_thread_safety () =
  let map = IntMap.create () in
  let num_keys = 100 in
  let num_domains = 4 in
  let ops_per_domain = 2000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      for _i = 0 to ops_per_domain - 1 do
        let key = Random.int num_keys in
        let value = Printf.sprintf "value_%d_%f" domain_id (Unix.gettimeofday ()) in
        
        (* Perform a sequence of operations that should be atomic *)
        if Random.bool () then (
          IntMap.insert map key value;
          assert (IntMap.has_key map key);
          match IntMap.get map key with
          | Some _ -> ()
          | None -> failwith "Key should exist after insertion"
        ) else (
          IntMap.remove map key;
          assert (not (IntMap.has_key map key) || IntMap.get map key <> None)
        )
      done
    )
  ) in
  
  Array.iter Domain.join domains;
  Printf.printf "✓ Thread safety property test passed\n"

let run_tests () =
  Printf.printf "Running Dashmap tests...\n\n";
  
  (* Basic functionality tests *)
  test_empty_map ();
  test_basic_operations ();
  test_multiple_values_same_key ();
  test_find_operations ();
  test_remove_operations ();
  test_iteration ();
  
  (* Concurrent tests *)
  test_concurrent_insertions ();
  test_concurrent_mixed_operations ();
  test_concurrent_readers_writers ();
  test_stress_operations ();
  
  (* Property tests *)
  test_property_thread_safety ();
  
  Printf.printf "\n✅ All Dashmap tests passed!\n"

let () = run_tests ()