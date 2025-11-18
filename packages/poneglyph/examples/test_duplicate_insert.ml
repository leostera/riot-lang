open Std

let () =
  println "=== Testing duplicate key insertion in SkipList ===\n";
  
  let module SL = Poneglyph.Storage.Lsm.Skiplist in
  
  (* Create a skiplist *)
  let t = SL.create () in
  
  (* Create a 41-byte key that starts with 0xFE like the problematic hash 
     We'll pad with zeros to make it 41 bytes *)
  let key_str = "FE8D5D99" ^ String.make (41 - 8) '0' in
  let key = String.to_bytes key_str in
  
  let value1 = String.to_bytes "value1" in
  let value2 = String.to_bytes "value2" in
  let value3 = String.to_bytes "value3" in
  
  println "Inserting key FE8D5D99... with value1 (first time)";
  let is_new1 = SL.insert t ~key ~value:value1 |> Result.expect ~msg:"insert 1" in
  println ("  Result: " ^ (if is_new1 then "NEW" else "UPDATE"));
  println ("  SkipList count: " ^ string_of_int (SL.count t));
  
  (* Try to find the value *)
  (match SL.find t ~key with
  | Some v -> 
      println ("  After insert 1: Found value = " ^ String.of_bytes v)
  | None -> 
      println "  After insert 1: Key not found!");
  
  println "\nInserting SAME key FE8D5D99... with value2 (second time - should UPDATE)";
  let is_new2 = SL.insert t ~key ~value:value2 |> Result.expect ~msg:"insert 2" in
  println ("  Result: " ^ (if is_new2 then "NEW" else "UPDATE") ^ " (expected: UPDATE)");
  println ("  SkipList count: " ^ string_of_int (SL.count t) ^ " (should still be 1)");
  
  (* Try to find the value - should be value2 now *)
  (match SL.find t ~key with
  | Some v -> 
      println ("  After insert 2: Found value = " ^ String.of_bytes v ^ " (expected: value2)")
  | None -> 
      println "  After insert 2: Key not found!");
  
  println "\nInserting SAME key FE8D5D99... with value3 (third time - should UPDATE)";
  let is_new3 = SL.insert t ~key ~value:value3 |> Result.expect ~msg:"insert 3" in
  println ("  Result: " ^ (if is_new3 then "NEW" else "UPDATE") ^ " (expected: UPDATE)");
  println ("  SkipList count: " ^ string_of_int (SL.count t) ^ " (should still be 1)");
  
  (* Try to find the value - should be value3 now *)
  (match SL.find t ~key with
  | Some v -> 
      println ("  After insert 3: Found value = " ^ String.of_bytes v ^ " (expected: value3)")
  | None -> 
      println "  After insert 3: Key not found!");
  
  (* Count how many keys we can iterate over *)
  println "\nIterating over skiplist...";
  let iter_count = ref 0 in
  SL.iter t ~f:(fun ~key:k ~value:v ->
    iter_count := !iter_count + 1;
    println ("  Found key: " ^ String.sub (String.of_bytes k) 0 4 ^ "..., value: " ^ String.of_bytes v)
  );
  
  println ("\nTotal keys found during iteration: " ^ string_of_int !iter_count ^ " (expected: 1)");
  println ("Total keys in skiplist: " ^ string_of_int (SL.count t) ^ " (expected: 1)");
  
  if SL.count t = 1 && !iter_count = 1 && not is_new2 && not is_new3 then
    println "\n✅ TEST PASSED: Duplicate detection working correctly"
  else begin
    println ("\n❌ TEST FAILED:");
    println ("  Count=" ^ string_of_int (SL.count t) ^ " (expected: 1)");
    println ("  Iterated=" ^ string_of_int !iter_count ^ " (expected: 1)");
    println ("  Insert2 was " ^ (if is_new2 then "NEW (WRONG)" else "UPDATE (correct)"));
    println ("  Insert3 was " ^ (if is_new3 then "NEW (WRONG)" else "UPDATE (correct)"));
    exit 1
  end
