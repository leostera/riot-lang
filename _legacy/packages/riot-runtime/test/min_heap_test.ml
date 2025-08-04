open Riot_runtime

(* Test module for Min_heap (Leftist Heap) *)

module IntHeap = Min_heap.Make_from_compare(struct
  type t = int
  let compare = Int.compare
end)

module StringHeap = Min_heap.Make_from_compare(struct
  type t = string
  let compare = String.compare
end)

let test_empty_heap () =
  let heap = IntHeap.empty in
  assert (IntHeap.is_empty heap);
  assert (IntHeap.find_min heap = None);
  assert (IntHeap.size heap = 0);
  assert (IntHeap.take heap = None);
  Printf.printf "✓ Empty heap test passed\n"

let test_single_element () =
  let heap = IntHeap.insert 42 IntHeap.empty in
  assert (not (IntHeap.is_empty heap));
  assert (IntHeap.find_min heap = Some 42);
  assert (IntHeap.find_min_exn heap = 42);
  assert (IntHeap.size heap = 1);
  
  let (remaining, min_val) = IntHeap.take_exn heap in
  assert (min_val = 42);
  assert (IntHeap.is_empty remaining);
  
  Printf.printf "✓ Single element test passed\n"

let test_min_heap_property () =
  let values = [5; 2; 8; 1; 9; 3; 7; 4; 6] in
  let heap = IntHeap.of_list values in
  
  (* Min should be 1 *)
  assert (IntHeap.find_min_exn heap = 1);
  
  (* Extract all elements in sorted order *)
  let sorted = IntHeap.to_list_sorted heap in
  let expected = List.sort Int.compare values in
  assert (sorted = expected);
  
  Printf.printf "✓ Min heap property test passed\n"

let test_heap_operations () =
  let heap = IntHeap.empty in
  
  (* Build heap: insert [10, 5, 15, 3, 8, 12, 20] *)
  let heap = IntHeap.insert 10 heap in
  let heap = IntHeap.insert 5 heap in
  let heap = IntHeap.insert 15 heap in
  let heap = IntHeap.insert 3 heap in
  let heap = IntHeap.insert 8 heap in
  let heap = IntHeap.insert 12 heap in
  let heap = IntHeap.insert 20 heap in
  
  assert (IntHeap.size heap = 7);
  assert (IntHeap.find_min_exn heap = 3);
  
  (* Take minimum *)
  let (heap, min1) = IntHeap.take_exn heap in
  assert (min1 = 3);
  assert (IntHeap.find_min_exn heap = 5);
  
  let (heap, min2) = IntHeap.take_exn heap in
  assert (min2 = 5);
  assert (IntHeap.find_min_exn heap = 8);
  
  Printf.printf "✓ Heap operations test passed\n"

let test_merge_heaps () =
  let heap1 = IntHeap.of_list [1; 3; 5; 7; 9] in
  let heap2 = IntHeap.of_list [2; 4; 6; 8; 10] in
  
  let merged = IntHeap.merge heap1 heap2 in
  assert (IntHeap.size merged = 10);
  assert (IntHeap.find_min_exn merged = 1);
  
  let sorted = IntHeap.to_list_sorted merged in
  assert (sorted = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  
  Printf.printf "✓ Merge heaps test passed\n"

let test_filter_operations () =
  let heap = IntHeap.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  
  (* Filter even numbers *)
  let even_heap = IntHeap.filter (fun x -> x mod 2 = 0) heap in
  let even_sorted = IntHeap.to_list_sorted even_heap in
  assert (even_sorted = [2; 4; 6; 8; 10]);
  
  (* Filter numbers > 5 *)
  let large_heap = IntHeap.filter (fun x -> x > 5) heap in
  let large_sorted = IntHeap.to_list_sorted large_heap in
  assert (large_sorted = [6; 7; 8; 9; 10]);
  
  Printf.printf "✓ Filter operations test passed\n"

let test_delete_operations () =
  let heap = IntHeap.of_list [1; 2; 2; 3; 2; 4; 5] in
  
  (* Delete one occurrence of 2 *)
  let heap_del_one = IntHeap.delete_one (=) 2 heap in
  let remaining_after_one = IntHeap.to_list_sorted heap_del_one in
  assert (List.length remaining_after_one = 6);
  assert (List.mem 2 remaining_after_one); (* Should still have 2s *)
  
  (* Delete all occurrences of 2 *)
  let heap_del_all = IntHeap.delete_all (=) 2 heap in
  let remaining_after_all = IntHeap.to_list_sorted heap_del_all in
  assert (remaining_after_all = [1; 3; 4; 5]);
  assert (not (List.mem 2 remaining_after_all));
  
  Printf.printf "✓ Delete operations test passed\n"

let test_iteration_and_folding () =
  let heap = IntHeap.of_list [3; 1; 4; 1; 5; 9; 2; 6] in
  
  (* Test iteration *)
  let collected = ref [] in
  IntHeap.iter (fun x -> collected := x :: !collected) heap;
  let sorted_collected = List.sort Int.compare !collected in
  let expected = List.sort Int.compare [3; 1; 4; 1; 5; 9; 2; 6] in
  assert (sorted_collected = expected);
  
  (* Test fold *)
  let sum = IntHeap.fold (+) 0 heap in
  let expected_sum = List.fold_left (+) 0 [3; 1; 4; 1; 5; 9; 2; 6] in
  assert (sum = expected_sum);
  
  Printf.printf "✓ Iteration and folding test passed\n"

let test_conversions () =
  let original_list = [7; 3; 8; 1; 9; 2; 5; 4; 6] in
  let heap = IntHeap.of_list original_list in
  
  (* Test to_list (unordered) *)
  let heap_list = IntHeap.to_list heap in
  let sorted_heap_list = List.sort Int.compare heap_list in
  let sorted_original = List.sort Int.compare original_list in
  assert (sorted_heap_list = sorted_original);
  
  (* Test to_list_sorted *)
  let sorted_list = IntHeap.to_list_sorted heap in
  assert (sorted_list = sorted_original);
  
  (* Test sequence operations *)
  let heap_from_seq = IntHeap.of_seq (List.to_seq original_list) in
  let seq_sorted = IntHeap.to_seq_sorted heap_from_seq |> List.of_seq in
  assert (seq_sorted = sorted_original);
  
  Printf.printf "✓ Conversions test passed\n"

let test_string_heap () =
  let words = ["zebra"; "apple"; "banana"; "cherry"; "date"] in
  let heap = StringHeap.of_list words in
  
  assert (StringHeap.find_min_exn heap = "apple");
  
  let sorted_words = StringHeap.to_list_sorted heap in
  let expected = List.sort String.compare words in
  assert (sorted_words = expected);
  
  Printf.printf "✓ String heap test passed\n"

let test_exception_handling () =
  let heap = IntHeap.empty in
  
  (* Test Empty exception *)
  (try
    ignore (IntHeap.find_min_exn heap);
    assert false (* Should not reach here *)
  with IntHeap.Empty -> ());
  
  (try
    ignore (IntHeap.take_exn heap);
    assert false (* Should not reach here *)
  with IntHeap.Empty -> ());
  
  Printf.printf "✓ Exception handling test passed\n"

let test_large_heap_performance () =
  let size = 10000 in
  let values = List.init size (fun i -> Random.int 100000) in
  
  (* Build heap *)
  let start_time = Unix.gettimeofday () in
  let heap = IntHeap.of_list values in
  let build_time = Unix.gettimeofday () -. start_time in
  
  assert (IntHeap.size heap = size);
  
  (* Extract all elements in sorted order *)
  let start_time = Unix.gettimeofday () in
  let sorted = IntHeap.to_list_sorted heap in
  let sort_time = Unix.gettimeofday () -. start_time in
  
  (* Verify sorting *)
  let manual_sorted = List.sort Int.compare values in
  assert (sorted = manual_sorted);
  
  Printf.printf "✓ Large heap performance test passed (build: %.3fs, sort: %.3fs)\n" 
    build_time sort_time

(* Property-based test: heap invariant *)
let test_property_heap_invariant () =
  let test_invariant heap =
    let rec check_invariant = function
      | IntHeap.E -> true
      | IntHeap.N (_, x, left, right) ->
          let left_min = match IntHeap.find_min left with
            | None -> true
            | Some min_val -> x <= min_val
          in
          let right_min = match IntHeap.find_min right with
            | None -> true  
            | Some min_val -> x <= min_val
          in
          left_min && right_min && check_invariant left && check_invariant right
    in
    check_invariant heap
  in
  
  (* Test with random insertions *)
  let heap = ref IntHeap.empty in
  for _i = 0 to 1000 do
    let value = Random.int 10000 in
    heap := IntHeap.insert value !heap;
    assert (test_invariant !heap)
  done;
  
  Printf.printf "✓ Heap invariant property test passed\n"

(* Stress test with random operations *)
let test_stress_random_operations () =
  let heap = ref IntHeap.empty in
  let reference_list = ref [] in
  let operations = 5000 in
  
  for _i = 0 to operations do
    match Random.int 3 with
    | 0 -> (* Insert *)
        let value = Random.int 1000 in
        heap := IntHeap.insert value !heap;
        reference_list := value :: !reference_list
    | 1 when not (IntHeap.is_empty !heap) -> (* Take *)
        let (new_heap, _min_val) = IntHeap.take_exn !heap in
        heap := new_heap;
        reference_list := List.tl (List.sort Int.compare !reference_list)
    | _ -> () (* Skip operation if heap is empty or random case *)
  done;
  
  (* Verify final state *)
  let heap_sorted = IntHeap.to_list_sorted !heap in
  let ref_sorted = List.sort Int.compare !reference_list in
  assert (heap_sorted = ref_sorted);
  
  Printf.printf "✓ Stress random operations test passed (%d ops)\n" operations

let run_tests () =
  Printf.printf "Running Min_heap tests...\n\n";
  
  (* Basic functionality tests *)
  test_empty_heap ();
  test_single_element ();
  test_min_heap_property ();
  test_heap_operations ();
  test_merge_heaps ();
  
  (* Advanced operations *)
  test_filter_operations ();
  test_delete_operations ();
  test_iteration_and_folding ();
  test_conversions ();
  
  (* Type variations *)
  test_string_heap ();
  
  (* Error handling *)
  test_exception_handling ();
  
  (* Performance and stress tests *)
  test_large_heap_performance ();
  test_stress_random_operations ();
  
  (* Property tests *)
  test_property_heap_invariant ();
  
  Printf.printf "\n✅ All Min_heap tests passed!\n"

let () = Random.self_init (); run_tests ()