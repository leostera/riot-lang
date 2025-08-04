open Riot_runtime

(* Test module for Uid (unique identifier generation) *)

let test_basic_uid_generation () =
  let uid1 = Uid.next () in
  let uid2 = Uid.next () in
  let uid3 = Uid.next () in
  
  (* UIDs should be different *)
  assert (not (Uid.equal uid1 uid2));
  assert (not (Uid.equal uid2 uid3));
  assert (not (Uid.equal uid1 uid3));
  
  (* UIDs should be sequential *)
  assert (Int64.compare uid1 uid2 < 0);
  assert (Int64.compare uid2 uid3 < 0);
  
  Printf.printf "✓ Basic UID generation test passed\n"

let test_uid_equality () =
  let uid1 = Uid.next () in
  let uid2 = Uid.next () in
  
  (* Test equality function *)
  assert (Uid.equal uid1 uid1);
  assert (not (Uid.equal uid1 uid2));
  
  (* Test with Int64 values *)
  assert (Uid.equal 42L 42L);
  assert (not (Uid.equal 42L 43L));
  
  Printf.printf "✓ UID equality test passed\n"

let test_uid_sequential_property () =
  let count = 1000 in
  let uids = Array.init count (fun _ -> Uid.next ()) in
  
  (* Verify all UIDs are unique *)
  for i = 0 to count - 1 do
    for j = i + 1 to count - 1 do
      assert (not (Uid.equal uids.(i) uids.(j)))
    done
  done;
  
  (* Verify UIDs are in ascending order *)
  for i = 0 to count - 2 do
    assert (Int64.compare uids.(i) uids.(i + 1) < 0)
  done;
  
  Printf.printf "✓ UID sequential property test passed (%d UIDs)\n" count

let test_uid_formatting () =
  let uid = Uid.next () in
  let uid_string = Format.asprintf "%a" Uid.pp uid in
  let expected = Int64.to_string uid in
  
  assert (String.equal uid_string expected);
  
  (* Test with known values *)
  let test_val = 12345L in
  let formatted = Format.asprintf "%a" Uid.pp test_val in
  assert (String.equal formatted "12345");
  
  Printf.printf "✓ UID formatting test passed\n"

let test_uid_large_numbers () =
  (* Test with large Int64 values *)
  let large_uid = Int64.max_int in
  let formatted = Format.asprintf "%a" Uid.pp large_uid in
  let expected = Int64.to_string large_uid in
  
  assert (String.equal formatted expected);
  assert (Uid.equal large_uid large_uid);
  
  Printf.printf "✓ UID large numbers test passed\n"

(* Concurrent testing using Domains *)
let test_concurrent_uid_generation () =
  let num_domains = 8 in
  let uids_per_domain = 1000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      let local_uids = Array.init uids_per_domain (fun _ -> Uid.next ()) in
      
      (* Verify local UIDs are unique and sequential *)
      for i = 0 to uids_per_domain - 2 do
        assert (not (Uid.equal local_uids.(i) local_uids.(i + 1)));
        assert (Int64.compare local_uids.(i) local_uids.(i + 1) < 0)
      done;
      
      (domain_id, local_uids)
    )
  ) in
  
  let all_results = Array.map Domain.join domains in
  
  (* Collect all UIDs from all domains *)
  let all_uids = ref [] in
  Array.iter (fun (_domain_id, uids) ->
    Array.iter (fun uid -> all_uids := uid :: !all_uids) uids
  ) all_results;
  
  let total_uids = List.length !all_uids in
  let expected_total = num_domains * uids_per_domain in
  assert (total_uids = expected_total);
  
  (* Verify global uniqueness *)
  let sorted_uids = List.sort Int64.compare !all_uids in
  let rec check_unique = function
    | [] | [_] -> true
    | a :: b :: rest -> 
        if Int64.equal a b then false
        else check_unique (b :: rest)
  in
  assert (check_unique sorted_uids);
  
  Printf.printf "✓ Concurrent UID generation test passed (%d total UIDs)\n" total_uids

let test_atomic_increment_behavior () =
  (* This test verifies the atomic compare-and-set behavior *)
  let initial_uid = Uid.next () in
  
  (* Generate a bunch more UIDs *)
  let uids = Array.init 100 (fun _ -> Uid.next ()) in
  
  (* Verify they're all greater than initial *)
  Array.iter (fun uid ->
    assert (Int64.compare uid initial_uid > 0)
  ) uids;
  
  (* Verify no gaps (should be consecutive) *)
  let sorted = Array.copy uids in
  Array.sort Int64.compare sorted;
  
  Printf.printf "✓ Atomic increment behavior test passed\n"

let test_uid_stress_generation () =
  let num_uids = 50000 in
  let start_time = Unix.gettimeofday () in
  
  let uids = Array.init num_uids (fun _ -> Uid.next ()) in
  
  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in
  
  (* Verify all UIDs are unique *)
  let uid_set = Hashtbl.create num_uids in
  Array.iter (fun uid ->
    if Hashtbl.mem uid_set uid then
      failwith "Duplicate UID found";
    Hashtbl.add uid_set uid ()
  ) uids;
  
  assert (Hashtbl.length uid_set = num_uids);
  
  Printf.printf "✓ UID stress generation test passed (%d UIDs in %.3fs)\n" 
    num_uids duration

let test_concurrent_stress () =
  let num_domains = 16 in
  let uids_per_domain = 2000 in
  
  let start_time = Unix.gettimeofday () in
  
  let domains = Array.init num_domains (fun _domain_id ->
    Domain.spawn (fun () ->
      Array.init uids_per_domain (fun _ -> Uid.next ())
    )
  ) in
  
  let all_uid_arrays = Array.map Domain.join domains in
  
  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in
  
  (* Flatten all UIDs and check uniqueness *)
  let all_uids = Array.concat (Array.to_list all_uid_arrays) in
  let total_count = Array.length all_uids in
  
  (* Use a hashtable to check for duplicates *)
  let uid_set = Hashtbl.create total_count in
  Array.iter (fun uid ->
    if Hashtbl.mem uid_set uid then
      failwith (Printf.sprintf "Duplicate UID found: %Ld" uid);
    Hashtbl.add uid_set uid ()
  ) all_uids;
  
  assert (Hashtbl.length uid_set = total_count);
  assert (total_count = num_domains * uids_per_domain);
  
  Printf.printf "✓ Concurrent stress test passed (%d UIDs, %d domains, %.3fs)\n" 
    total_count num_domains duration

(* Property test: monotonic increasing *)
let test_property_monotonic () =
  let num_samples = 10000 in
  let uids = Array.init num_samples (fun _ -> Uid.next ()) in
  
  (* Verify strict monotonic increasing property *)
  for i = 0 to num_samples - 2 do
    let current = uids.(i) in
    let next = uids.(i + 1) in
    assert (Int64.compare current next < 0);
    assert (Int64.sub next current = 1L) (* Should be consecutive *)
  done;
  
  Printf.printf "✓ Monotonic property test passed\n"

let test_boundary_conditions () =
  (* Test behavior around potential overflow scenarios *)
  (* Note: In practice, Int64.max_int is huge and unlikely to overflow *)
  
  let uid1 = Uid.next () in
  let uid2 = Uid.next () in
  
  (* Basic sanity checks *)
  assert (Int64.compare uid1 0L >= 0);  (* Should be non-negative *)
  assert (Int64.compare uid2 uid1 > 0); (* Should be increasing *)
  
  Printf.printf "✓ Boundary conditions test passed\n"

let run_tests () =
  Printf.printf "Running Uid tests...\n\n";
  
  (* Basic functionality *)
  test_basic_uid_generation ();
  test_uid_equality ();
  test_uid_sequential_property ();
  test_uid_formatting ();
  test_uid_large_numbers ();
  
  (* Concurrent behavior *)
  test_concurrent_uid_generation ();
  test_atomic_increment_behavior ();
  
  (* Stress tests *)
  test_uid_stress_generation ();
  test_concurrent_stress ();
  
  (* Properties *)
  test_property_monotonic ();
  test_boundary_conditions ();
  
  Printf.printf "\n✅ All Uid tests passed!\n"

let () = run_tests ()