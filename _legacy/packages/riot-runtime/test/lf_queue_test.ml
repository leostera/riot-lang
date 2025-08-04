open Riot_runtime

(* Property-based testing for lock-free queue *)

let test_empty_queue () =
  let q = Lf_queue.create () in
  assert (Lf_queue.is_empty q);
  assert (Lf_queue.pop q = None);
  Printf.printf "✓ Empty queue test passed\n"

let test_single_push_pop () =
  let q = Lf_queue.create () in
  Lf_queue.push q 42;
  assert (not (Lf_queue.is_empty q));
  assert (Lf_queue.pop q = Some 42);
  assert (Lf_queue.is_empty q);
  assert (Lf_queue.pop q = None);
  Printf.printf "✓ Single push/pop test passed\n"

let test_fifo_order () =
  let q = Lf_queue.create () in
  let values = [1; 2; 3; 4; 5] in
  List.iter (Lf_queue.push q) values;
  
  let rec pop_all acc =
    match Lf_queue.pop q with
    | None -> List.rev acc
    | Some v -> pop_all (v :: acc)
  in
  let popped = pop_all [] in
  assert (popped = values);
  Printf.printf "✓ FIFO order test passed\n"

let test_push_head_fifo () =
  let q = Lf_queue.create () in
  (* Push to head should reverse order *)
  Lf_queue.push_head q 1;
  Lf_queue.push_head q 2;
  Lf_queue.push_head q 3;
  
  assert (Lf_queue.pop q = Some 3);
  assert (Lf_queue.pop q = Some 2);
  assert (Lf_queue.pop q = Some 1);
  assert (Lf_queue.pop q = None);
  Printf.printf "✓ Push head FIFO test passed\n"

let test_mixed_push_operations () =
  let q = Lf_queue.create () in
  Lf_queue.push q 1;          (* [1] *)
  Lf_queue.push_head q 2;     (* [2, 1] *)
  Lf_queue.push q 3;          (* [2, 1, 3] *)
  Lf_queue.push_head q 4;     (* [4, 2, 1, 3] *)
  
  assert (Lf_queue.pop q = Some 4);
  assert (Lf_queue.pop q = Some 2);
  assert (Lf_queue.pop q = Some 1);
  assert (Lf_queue.pop q = Some 3);
  assert (Lf_queue.pop q = None);
  Printf.printf "✓ Mixed push operations test passed\n"

let test_peek () =
  let q = Lf_queue.create () in
  Lf_queue.push q 42;
  let head_val = Lf_queue.peek q in
  (* Should not modify queue *)
  assert (not (Lf_queue.is_empty q));
  assert (Lf_queue.pop q = Some 42);
  assert (Lf_queue.is_empty q);
  Printf.printf "✓ Peek test passed\n"

let test_close_queue () =
  let q = Lf_queue.create () in
  Lf_queue.push q 1;
  Lf_queue.push q 2;
  
  (* Close the queue *)
  Lf_queue.close q;
  
  (* Should still be able to pop existing items *)
  assert (Lf_queue.pop q = Some 1);
  assert (Lf_queue.pop q = Some 2);
  assert (Lf_queue.pop q = None);
  
  (* Should raise Closed exception on new pushes *)
  (try
    Lf_queue.push q 3;
    assert false (* Should not reach here *)
  with Lf_queue.Closed -> ());
  
  (try
    Lf_queue.push_head q 4;
    assert false (* Should not reach here *)
  with Lf_queue.Closed -> ());
  
  Printf.printf "✓ Close queue test passed\n"

(* Concurrent testing - using Domains *)
let test_concurrent_push_pop () =
  let q = Lf_queue.create () in
  let num_producers = 4 in
  let num_items_per_producer = 100 in
  let total_items = num_producers * num_items_per_producer in
  
  let producers = Array.init num_producers (fun producer_id ->
    Domain.spawn (fun () ->
      for i = 0 to num_items_per_producer - 1 do
        let value = producer_id * num_items_per_producer + i in
        Lf_queue.push q value
      done
    )
  ) in
  
  (* Consumer domain *)
  let consumer = Domain.spawn (fun () ->
    let rec consume acc count =
      if count >= total_items then acc
      else
        match Lf_queue.pop q with
        | None -> consume acc count
        | Some v -> consume (v :: acc) (count + 1)
    in
    consume [] 0
  ) in
  
  (* Wait for all producers *)
  Array.iter Domain.join producers;
  
  (* Wait for consumer *)
  let consumed = Domain.join consumer in
  
  (* Verify all items were consumed *)
  assert (List.length consumed = total_items);
  let consumed_sorted = List.sort Int.compare consumed in
  let expected = List.init total_items (fun i -> i) in
  assert (consumed_sorted = expected);
  
  Printf.printf "✓ Concurrent push/pop test passed (%d items)\n" total_items

let test_stress_concurrent () =
  let q = Lf_queue.create () in
  let num_domains = 8 in
  let operations_per_domain = 1000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      let local_count = ref 0 in
      for i = 0 to operations_per_domain - 1 do
        if Random.bool () then (
          (* Push operation *)
          let value = domain_id * operations_per_domain + i in
          Lf_queue.push q value;
          incr local_count
        ) else (
          (* Pop operation *)
          match Lf_queue.pop q with
          | None -> ()
          | Some _ -> decr local_count
        )
      done;
      !local_count
    )
  ) in
  
  let final_counts = Array.map Domain.join domains in
  let total_remaining = Array.fold_left (+) 0 final_counts in
  
  (* Count remaining items in queue *)
  let rec count_remaining acc =
    match Lf_queue.pop q with
    | None -> acc
    | Some _ -> count_remaining (acc + 1)
  in
  let queue_remaining = count_remaining 0 in
  
  assert (queue_remaining = total_remaining);
  Printf.printf "✓ Stress concurrent test passed (remaining: %d)\n" total_remaining

(* Property: Queue maintains FIFO order under sequential operations *)
let test_property_sequential_fifo () =
  let q = Lf_queue.create () in
  let test_data = Array.init 1000 (fun i -> i) in
  
  (* Push all items *)
  Array.iter (Lf_queue.push q) test_data;
  
  (* Pop all items and verify order *)
  for i = 0 to Array.length test_data - 1 do
    match Lf_queue.pop q with
    | None -> assert false
    | Some v -> assert (v = test_data.(i))
  done;
  
  assert (Lf_queue.is_empty q);
  Printf.printf "✓ Sequential FIFO property test passed\n"

(* Property: No data races in concurrent access *)
let test_property_no_data_races () =
  let q = Lf_queue.create () in
  let num_ops = 10000 in
  
  let producer1 = Domain.spawn (fun () ->
    for i = 0 to num_ops - 1 do
      Lf_queue.push q (i * 2)  (* Even numbers *)
    done
  ) in
  
  let producer2 = Domain.spawn (fun () ->
    for i = 0 to num_ops - 1 do
      Lf_queue.push q (i * 2 + 1)  (* Odd numbers *)
    done
  ) in
  
  let consumer = Domain.spawn (fun () ->
    let rec consume acc =
      match Lf_queue.pop q with
      | None -> acc
      | Some v -> consume (v :: acc)
    in
    let rec consume_all acc total =
      if total >= num_ops * 2 then acc
      else
        let new_items = consume [] in
        consume_all (new_items @ acc) (total + List.length new_items)
    in
    consume_all [] 0
  ) in
  
  Domain.join producer1;
  Domain.join producer2;
  let consumed = Domain.join consumer in
  
  (* Verify all items were consumed exactly once *)
  let consumed_sorted = List.sort Int.compare consumed in
  let expected = List.init (num_ops * 2) (fun i -> i) in
  assert (consumed_sorted = expected);
  
  Printf.printf "✓ No data races property test passed\n"

let run_tests () =
  Printf.printf "Running Lf_queue tests...\n\n";
  
  (* Basic functionality tests *)
  test_empty_queue ();
  test_single_push_pop ();
  test_fifo_order ();
  test_push_head_fifo ();
  test_mixed_push_operations ();
  test_peek ();
  test_close_queue ();
  
  (* Property-based tests *)
  test_property_sequential_fifo ();
  
  (* Concurrent tests *)
  test_concurrent_push_pop ();
  test_stress_concurrent ();
  test_property_no_data_races ();
  
  Printf.printf "\n✅ All Lf_queue tests passed!\n"

let () = run_tests ()