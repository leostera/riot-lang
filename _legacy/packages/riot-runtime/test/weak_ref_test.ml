open Riot_runtime

(* Test module for Weak_ref *)

let test_basic_weak_ref () =
  let value = [1; 2; 3; 4; 5] in
  let weak_ref = Weak_ref.make value in
  
  (* Should be able to retrieve the value initially *)
  assert (Weak_ref.get weak_ref = Some value);
  
  Printf.printf "✓ Basic weak ref test passed\n"

let test_weak_ref_with_strings () =
  let str = "Hello, Riot!" in
  let weak_ref = Weak_ref.make str in
  
  match Weak_ref.get weak_ref with
  | Some retrieved -> assert (String.equal retrieved str)
  | None -> assert false
  
  Printf.printf "✓ Weak ref with strings test passed\n"

let test_weak_ref_with_records () =
  type test_record = { id: int; name: string; data: float list }
  
  let record = { id = 42; name = "test"; data = [1.0; 2.0; 3.0] } in
  let weak_ref = Weak_ref.make record in
  
  match Weak_ref.get weak_ref with
  | Some retrieved -> 
      assert (retrieved.id = record.id);
      assert (String.equal retrieved.name record.name);
      assert (retrieved.data = record.data)
  | None -> assert false
  
  Printf.printf "✓ Weak ref with records test passed\n"

let test_weak_ref_gc_behavior () =
  (* Create a value that can be GC'd *)
  let create_large_value () = 
    Array.init 10000 (fun i -> Printf.sprintf "value_%d" i)
  in
  
  let weak_ref = Weak_ref.make (create_large_value ()) in
  
  (* Initially should be available *)
  assert (Weak_ref.get weak_ref <> None);
  
  (* Force garbage collection multiple times *)
  for _i = 0 to 10 do
    Gc.full_major ();
    Gc.compact ()
  done;
  
  (* After GC, the value might or might not be collected *)
  (* This is non-deterministic, so we just verify the operation doesn't crash *)
  let result = Weak_ref.get weak_ref in
  match result with
  | Some _value -> Printf.printf "✓ Value survived GC\n"
  | None -> Printf.printf "✓ Value was collected by GC\n";
  
  Printf.printf "✓ Weak ref GC behavior test passed\n"

let test_multiple_weak_refs () =
  let values = [("a", 1); ("b", 2); ("c", 3); ("d", 4)] in
  let weak_refs = List.map (fun pair -> (pair, Weak_ref.make pair)) values in
  
  (* Verify all weak refs initially work *)
  List.iter (fun (original, weak_ref) ->
    match Weak_ref.get weak_ref with
    | Some retrieved -> assert (retrieved = original)
    | None -> assert false
  ) weak_refs;
  
  Printf.printf "✓ Multiple weak refs test passed\n"

let test_weak_ref_with_functions () =
  let func = fun x -> x * 2 + 1 in
  let weak_ref = Weak_ref.make func in
  
  match Weak_ref.get weak_ref with
  | Some retrieved_func -> 
      assert (retrieved_func 5 = 11);
      assert (retrieved_func 10 = 21)
  | None -> assert false
  
  Printf.printf "✓ Weak ref with functions test passed\n"

let test_weak_ref_none_after_scope () =
  let weak_ref = ref None in
  
  (* Create a local scope where the value exists *)
  begin
    let local_value = String.make 10000 'x' in  (* Large string *)
    weak_ref := Some (Weak_ref.make local_value)
  end;
  
  (* Force GC after the value goes out of scope *)
  for _i = 0 to 5 do
    Gc.full_major ();
    Gc.compact ()
  done;
  
  (* The weak reference might now return None *)
  match !weak_ref with
  | Some wr -> 
      (match Weak_ref.get wr with
      | Some _v -> Printf.printf "✓ Value unexpectedly survived scope\n"
      | None -> Printf.printf "✓ Value correctly collected after scope\n")
  | None -> assert false
  
  Printf.printf "✓ Weak ref after scope test passed\n"

(* Concurrent testing with Domains *)
let test_concurrent_weak_refs () =
  let num_domains = 4 in
  let refs_per_domain = 1000 in
  
  let domains = Array.init num_domains (fun domain_id ->
    Domain.spawn (fun () ->
      let local_refs = ref [] in
      for i = 0 to refs_per_domain - 1 do
        let value = (domain_id, i, Printf.sprintf "data_%d_%d" domain_id i) in
        let weak_ref = Weak_ref.make value in
        local_refs := weak_ref :: !local_refs
      done;
      
      (* Verify all refs initially work *)
      let successful = ref 0 in
      List.iteri (fun idx weak_ref ->
        match Weak_ref.get weak_ref with
        | Some (d_id, i, _) when d_id = domain_id && i = (refs_per_domain - 1 - idx) ->
            incr successful
        | _ -> ()
      ) !local_refs;
      !successful
    )
  ) in
  
  let successful_counts = Array.map Domain.join domains in
  let total_successful = Array.fold_left (+) 0 successful_counts in
  let total_expected = num_domains * refs_per_domain in
  
  assert (total_successful = total_expected);
  Printf.printf "✓ Concurrent weak refs test passed (%d/%d successful)\n" 
    total_successful total_expected

let test_weak_ref_stress () =
  let num_operations = 10000 in
  let weak_refs = ref [] in
  
  (* Create many weak references *)
  for i = 0 to num_operations - 1 do
    let value = (i, Printf.sprintf "stress_test_%d" i, Random.float 1000.0) in
    let weak_ref = Weak_ref.make value in
    weak_refs := weak_ref :: !weak_refs
  done;
  
  (* Trigger some GC *)
  Gc.major ();
  
  (* Check how many are still alive *)
  let alive_count = ref 0 in
  List.iter (fun weak_ref ->
    match Weak_ref.get weak_ref with
    | Some _ -> incr alive_count
    | None -> ()
  ) !weak_refs;
  
  Printf.printf "✓ Stress test passed (%d/%d refs still alive)\n" 
    !alive_count num_operations

let test_weak_ref_type_safety () =
  (* Test that weak refs maintain type safety *)
  let int_ref = Weak_ref.make 42 in
  let string_ref = Weak_ref.make "hello" in
  let list_ref = Weak_ref.make [1; 2; 3] in
  
  (* Each should return the correct type *)
  (match Weak_ref.get int_ref with
  | Some n -> assert (n = 42)
  | None -> assert false);
  
  (match Weak_ref.get string_ref with
  | Some s -> assert (String.equal s "hello")
  | None -> assert false);
  
  (match Weak_ref.get list_ref with
  | Some lst -> assert (lst = [1; 2; 3])
  | None -> assert false);
  
  Printf.printf "✓ Type safety test passed\n"

let run_tests () =
  Printf.printf "Running Weak_ref tests...\n\n";
  
  (* Basic functionality *)
  test_basic_weak_ref ();
  test_weak_ref_with_strings ();
  test_weak_ref_with_records ();
  test_weak_ref_with_functions ();
  
  (* GC behavior *)
  test_weak_ref_gc_behavior ();
  test_weak_ref_none_after_scope ();
  
  (* Multiple refs *)
  test_multiple_weak_refs ();
  
  (* Concurrent testing *)
  test_concurrent_weak_refs ();
  
  (* Stress testing *)
  test_weak_ref_stress ();
  
  (* Type safety *)
  test_weak_ref_type_safety ();
  
  Printf.printf "\n✅ All Weak_ref tests passed!\n"

let () = Random.self_init (); run_tests ()