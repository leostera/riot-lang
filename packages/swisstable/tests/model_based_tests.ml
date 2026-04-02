(** Model-Based Property Tests for SwissTable
    
    Uses HashMap as a reference implementation to verify behavioral equivalence.
    This is the gold standard for correctness - if Swisstable behaves identically
    to HashMap on random operations, we know it's correct.
*)
open Std
open Propane

(** {1 Test Strategy}
    
    We generate random sequences of operations and apply them to both
    Swisstable and HashMap, then verify they produce identical results.
    This catches subtle bugs that unit tests might miss. *)

(** {1 Model-Based Properties} *)

(* Property 1: Insert operations produce identical results *)

let insert_equivalence_prop =
  property "insert: swisstable matches hashmap" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 100);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Apply all inserts to both *)
      let all_match =
        List.for_all
          (fun ((k, v)) ->
            let r1 = Swisstable.insert swiss k v in
            let r2 = Collections.HashMap.insert hash k v in
            r1 = r2)
          pairs
      in
      if not all_match then
        fail "Insert return values differ";
      if not (Swisstable.len swiss = Collections.HashMap.len hash) then
        fail "Lengths differ after inserts";
      List.for_all (fun ((k, _)) -> Swisstable.get swiss k = Collections.HashMap.get hash k) pairs)

(* Property 2: Get operations produce identical results *)

let get_equivalence_prop =
  property "get: swisstable matches hashmap" Arbitrary.(pair (list (pair int int)) (list int))
    (fun ((insert_pairs, get_keys)) ->
      assume (Collections.List.length insert_pairs <= 50);
      assume (Collections.List.length get_keys <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        insert_pairs;
      (* Verify gets match *)
      List.for_all (fun k -> Swisstable.get swiss k = Collections.HashMap.get hash k) get_keys)

(* Property 3: Remove operations produce identical results *)

let remove_equivalence_prop =
  property "remove: swisstable matches hashmap" Arbitrary.(pair (list (pair int int)) (list int))
    (fun ((insert_pairs, remove_keys)) ->
      assume (Collections.List.length insert_pairs <= 50);
      assume (Collections.List.length remove_keys <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        insert_pairs;
      (* Verify removes match *)
      let all_match =
        List.for_all
          (fun k ->
            let r1 = Swisstable.remove swiss k in
            let r2 = Collections.HashMap.remove hash k in
            r1 = r2)
          remove_keys
      in
      if not all_match then
        fail "Remove return values differ";
      Swisstable.len swiss = Collections.HashMap.len hash)

(* Property 4: Contains operations produce identical results *)

let contains_equivalence_prop =
  property "contains_key: swisstable matches hashmap" Arbitrary.(pair
    (list (pair int int))
    (list int))
    (fun ((insert_pairs, check_keys)) ->
      assume (Collections.List.length insert_pairs <= 50);
      assume (Collections.List.length check_keys <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        insert_pairs;
      (* Verify contains_key matches *)
      List.for_all
        (fun k -> Swisstable.contains_key swiss k = Collections.HashMap.contains_key hash k)
        check_keys)

(* Property 5: Clear produces identical results *)

let clear_equivalence_prop =
  property "clear: swisstable matches hashmap" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      (* Clear both *)
      Swisstable.clear swiss;
      Collections.HashMap.clear hash;
      (* Verify both empty *)
      Swisstable.len swiss = 0
      && Collections.HashMap.len hash = 0
      && Swisstable.is_empty swiss = Collections.HashMap.is_empty hash)

(* Property 6: to_list produces same entries (modulo order) *)

let to_list_equivalence_prop =
  property "to_list: swisstable matches hashmap (unordered)" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      let swiss_list = Swisstable.to_list swiss in
      let hash_list = Collections.HashMap.to_list hash in
      (* Same length *)
      if not (Collections.List.length swiss_list = Collections.List.length hash_list) then
        fail "to_list lengths differ";
      List.for_all
        (fun entry ->
          List.mem entry hash_list)
        swiss_list)

(* Property 7: keys produce same keys (modulo order) *)

let keys_equivalence_prop =
  property "keys: swisstable matches hashmap (unordered)" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      let swiss_keys = Swisstable.keys swiss in
      let hash_keys = Collections.HashMap.keys hash in
      (* Same length *)
      if not (Collections.List.length swiss_keys = Collections.List.length hash_keys) then
        fail "keys lengths differ";
      List.for_all
        (fun k ->
          List.mem k hash_keys)
        swiss_keys)

(* Property 8: values produce same values (modulo order) *)

let values_equivalence_prop =
  property "values: swisstable matches hashmap (unordered)" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      let swiss_values = Swisstable.values swiss in
      let hash_values = Collections.HashMap.values hash in
      (* Same length *)
      if not (Collections.List.length swiss_values = Collections.List.length hash_values) then
        fail "values lengths differ";
      let sorted_swiss = List.sort compare swiss_values in
      let sorted_hash = List.sort compare hash_values in
      sorted_swiss = sorted_hash)

(* Property 9: fold produces identical results *)

let fold_equivalence_prop =
  property "fold: swisstable matches hashmap" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      (* Count entries *)
      let swiss_count =
        Swisstable.fold (fun _ _ acc -> acc + 1) swiss 0
      in
      let hash_count =
        Collections.HashMap.fold (fun _ _ acc -> acc + 1) hash 0
      in
      if not (swiss_count = hash_count) then
        fail "fold counts differ";
      let swiss_sum =
        Swisstable.fold (fun _ v acc -> acc + v) swiss 0
      in
      let hash_sum =
        Collections.HashMap.fold (fun _ v acc -> acc + v) hash 0
      in
      swiss_sum = hash_sum)

(* Property 10: or_insert produces identical results *)

let or_insert_equivalence_prop =
  property "or_insert: swisstable matches hashmap" Arbitrary.(triple int int (list (pair int int)))
    (fun ((key, default, pairs)) ->
      assume (Collections.List.length pairs <= 50);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      (* Test or_insert *)
      let r1 = Swisstable.or_insert swiss key default in
      let r2 = Collections.HashMap.or_insert hash key default in
      if not (r1 = r2) then
        fail "or_insert return values differ";
      Swisstable.get swiss key = Collections.HashMap.get hash key)

(** {1 Large-Scale Model Tests} *)

(* Property 11: Many operations maintain equivalence *)

let many_ops_equivalence_prop =
  property "many operations: swisstable matches hashmap" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs > 0);
      assume (Collections.List.length pairs <= 200);
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert all *)
      List.iter
        (fun ((k, v)) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash k v in
          ())
        pairs;
      (* Remove half *)
      let keys_to_remove =
        List.filteri (fun i _ -> i mod 2 = 0) pairs
      in
      List.iter
        (fun ((k, _)) ->
          let _ = Swisstable.remove swiss k in
          let _ = Collections.HashMap.remove hash k in
          ())
        keys_to_remove;
      (* Verify lengths match *)
      if not (Swisstable.len swiss = Collections.HashMap.len hash) then
        fail "Lengths differ after mixed operations";
      List.for_all (fun ((k, _)) -> Swisstable.get swiss k = Collections.HashMap.get hash k) pairs)

(* Property 12: Resize maintains equivalence *)

let resize_equivalence_prop =
  property "resize: swisstable matches hashmap" Arbitrary.int
    (fun _seed ->
      let swiss = Swisstable.with_capacity 1 in
      (* Force resizes *)
      let hash = Collections.HashMap.with_capacity 1 in
      (* Insert enough to trigger multiple resizes *)
      for i = 0 to 100 do
        let _ = Swisstable.insert swiss i (i * 2) in
        let _ = Collections.HashMap.insert hash i (i * 2) in
        ()
      done;
      (* Verify all entries match *)
      let all_match = List.init
        101
        (fun i -> Swisstable.get swiss i = Collections.HashMap.get hash i)
      |> List.for_all (fun x -> x) in
      if not all_match then
        fail "Entries differ after resize";
      Swisstable.len swiss = Collections.HashMap.len hash)

(** {1 Test Suite} *)

let tests = [
  insert_equivalence_prop;
  get_equivalence_prop;
  remove_equivalence_prop;
  contains_equivalence_prop;
  clear_equivalence_prop;
  to_list_equivalence_prop;
  keys_equivalence_prop;
  values_equivalence_prop;
  fold_equivalence_prop;
  or_insert_equivalence_prop;
  many_ops_equivalence_prop;
  resize_equivalence_prop;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"swisstable-model-based-tests" ~tests ~args)
    ~args:Env.args
    ()
