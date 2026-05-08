(**
   Model-Based Property Tests for SwissTable

   Uses HashMap as a reference implementation to verify behavioral equivalence.
   This is the gold standard for correctness - if Swisstable behaves identically
   to HashMap on random operations, we know it's correct.
*)
open Std
open Propane

let bounded_list_arb = fun ?(min = 0) max elem_arb ->
  let list_arb = Arbitrary.list elem_arb in
  { list_arb with gen = Generator.list_size (Generator.int_range min max) elem_arb.gen }

let non_empty_bounded_list_arb = fun max elem_arb -> bounded_list_arb ~min:1 max elem_arb

(**
   {1 Test Strategy}

   We generate random sequences of operations and apply them to both
   Swisstable and HashMap, then verify they produce identical results.
   This catches subtle bugs that unit tests might miss.
*)

(** {1 Model-Based Properties} *)

(* Property 1: Insert operations produce identical results *)

let insert_equivalence_prop =
  property
    "insert: swisstable matches hashmap"
    (bounded_list_arb 100 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Apply all inserts to both *)
      let all_match =
        List.for_all
          (fun (k, v) ->
            let r1 = Swisstable.insert swiss k v in
            let r2 = Collections.HashMap.insert hash ~key:k ~value:v in
            r1 = r2)
          pairs
      in
      if not all_match then
        fail "Insert return values differ";
      if not (Swisstable.len swiss = Collections.HashMap.length hash) then
        fail "Lengths differ after inserts";
      List.for_all
        (fun (k, _) -> Swisstable.get swiss k = Collections.HashMap.get hash ~key:k)
        pairs)

(* Property 2: Get operations produce identical results *)

let get_equivalence_prop =
  property
    "get: swisstable matches hashmap"
    (Arbitrary.pair
      (bounded_list_arb 50 Arbitrary.(pair int int))
      (bounded_list_arb 50 Arbitrary.int))
    (fun (insert_pairs, get_keys) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        insert_pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Verify gets match *)
      List.for_all (fun k -> Swisstable.get swiss k = Collections.HashMap.get hash ~key:k) get_keys)

(* Property 3: Remove operations produce identical results *)

let remove_equivalence_prop =
  property
    "remove: swisstable matches hashmap"
    (Arbitrary.pair
      (bounded_list_arb 50 Arbitrary.(pair int int))
      (bounded_list_arb 50 Arbitrary.int))
    (fun (insert_pairs, remove_keys) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        insert_pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Verify removes match *)
      let all_match =
        List.for_all
          (fun k ->
            let r1 = Swisstable.remove swiss k in
            let r2 = Collections.HashMap.remove hash ~key:k in
            r1 = r2)
          remove_keys
      in
      if not all_match then
        fail "Remove return values differ";
      Swisstable.len swiss = Collections.HashMap.length hash)

(* Property 4: Contains operations produce identical results *)

let contains_equivalence_prop =
  property
    "contains_key: swisstable matches hashmap"
    (Arbitrary.pair
      (bounded_list_arb 50 Arbitrary.(pair int int))
      (bounded_list_arb 50 Arbitrary.int))
    (fun (insert_pairs, check_keys) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        insert_pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Verify contains_key matches *)
      List.for_all
        (fun k -> Swisstable.contains_key swiss k = Collections.HashMap.has_key hash ~key:k)
        check_keys)

(* Property 5: Clear produces identical results *)

let clear_equivalence_prop =
  property
    "clear: swisstable matches hashmap"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Clear both *)
      Swisstable.clear swiss;
      Collections.HashMap.clear hash;
      (* Verify both empty *)
      Swisstable.len swiss = 0
      && Collections.HashMap.length hash = 0
      && Swisstable.is_empty swiss = Collections.HashMap.is_empty hash)

(* Property 6: to_list produces same entries (modulo order) *)

let to_list_equivalence_prop =
  property
    "to_list: swisstable matches hashmap (unordered)"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      let swiss_list = Swisstable.to_list swiss in
      let hash_list = Collections.HashMap.to_list hash in
      (* Same length *)
      if not (Collections.List.length swiss_list = Collections.List.length hash_list) then
        fail "to_list lengths differ";
      List.for_all (fun entry -> List.contains hash_list ~value:entry) swiss_list)

(* Property 7: keys produce same keys (modulo order) *)

let keys_equivalence_prop =
  property
    "keys: swisstable matches hashmap (unordered)"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      let swiss_keys = Swisstable.keys swiss in
      let hash_keys = Collections.HashMap.keys hash in
      (* Same length *)
      if not (Collections.List.length swiss_keys = Collections.List.length hash_keys) then
        fail "keys lengths differ";
      List.for_all (fun k -> List.contains hash_keys ~value:k) swiss_keys)

(* Property 8: values produce same values (modulo order) *)

let values_equivalence_prop =
  property
    "values: swisstable matches hashmap (unordered)"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      let swiss_values = Swisstable.values swiss in
      let hash_values = Collections.HashMap.values hash in
      (* Same length *)
      if not (Collections.List.length swiss_values = Collections.List.length hash_values) then
        fail "values lengths differ";
      let sorted_swiss = List.sort swiss_values ~compare in
      let sorted_hash = List.sort hash_values ~compare in
      sorted_swiss = sorted_hash)

(* Property 9: fold produces identical results *)

let fold_equivalence_prop =
  property
    "fold: swisstable matches hashmap"
    (bounded_list_arb 50 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Count entries *)
      let swiss_count = Swisstable.fold (fun _ _ acc -> acc + 1) swiss 0 in
      let hash_count = Collections.HashMap.fold_left hash ~init:0 ~fn:(fun acc _ _ -> acc + 1) in
      if not (swiss_count = hash_count) then
        fail "fold counts differ";
      let swiss_sum = Swisstable.fold (fun _ v acc -> acc + v) swiss 0 in
      let hash_sum =
        Collections.HashMap.fold_left hash ~init:0 ~fn:(fun acc _ value -> acc + value)
      in
      swiss_sum = hash_sum)

(* Property 10: or_insert produces identical results *)

let or_insert_equivalence_prop =
  property
    "or_insert: swisstable matches hashmap"
    (Arbitrary.triple
      Arbitrary.int
      Arbitrary.int
      (bounded_list_arb 50 Arbitrary.(pair int int)))
    (fun (key, default, pairs) ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Populate both *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Compare Swisstable.or_insert against the explicit HashMap get/insert flow. *)
      let r1 = Swisstable.or_insert swiss key default in
      let r2 =
        match Collections.HashMap.get hash ~key with
        | Some value -> value
        | None ->
            let _ = Collections.HashMap.insert hash ~key ~value:default in
            default
      in
      if not (r1 = r2) then
        fail "or_insert return values differ";
      Swisstable.get swiss key = Collections.HashMap.get hash ~key)

(** {1 Large-Scale Model Tests} *)

(* Property 11: Many operations maintain equivalence *)

let many_ops_equivalence_prop =
  property
    "many operations: swisstable matches hashmap"
    (non_empty_bounded_list_arb 200 Arbitrary.(pair int int))
    (fun pairs ->
      let swiss = Swisstable.create () in
      let hash = Collections.HashMap.create () in
      (* Insert all *)
      List.for_each
        pairs
        ~fn:(fun (k, v) ->
          let _ = Swisstable.insert swiss k v in
          let _ = Collections.HashMap.insert hash ~key:k ~value:v in
          ());
      (* Remove half *)
      let keys_to_remove =
        List.enumerate pairs
        |> List.filter_map
          ~fn:(fun (index, pair) ->
            if index mod 2 = 0 then
              Some pair
            else
              None)
      in
      List.for_each
        keys_to_remove
        ~fn:(fun (k, _) ->
          let _ = Swisstable.remove swiss k in
          let _ = Collections.HashMap.remove hash ~key:k in
          ());
      (* Verify lengths match *)
      if not (Swisstable.len swiss = Collections.HashMap.length hash) then
        fail "Lengths differ after mixed operations";
      List.for_all
        (fun (k, _) -> Swisstable.get swiss k = Collections.HashMap.get hash ~key:k)
        pairs)

(* Property 12: Resize maintains equivalence *)

let resize_equivalence_prop =
  property
    "resize: swisstable matches hashmap"
    Arbitrary.int
    (fun _seed ->
      let swiss = Swisstable.with_capacity 1 in
      (* Force resizes *)
      let hash = Collections.HashMap.with_capacity ~size:1 in
      (* Insert enough to trigger multiple resizes *)
      for i = 0 to 100 do
        let _ = Swisstable.insert swiss i (i * 2) in
        let _ = Collections.HashMap.insert hash ~key:i ~value:(i * 2) in
        ()
      done;
      (* Verify all entries match *)
      let all_match =
        List.init
          ~count:101
          ~fn:(fun i -> Swisstable.get swiss i = Collections.HashMap.get hash ~key:i)
        |> List.for_all (fun x -> x)
      in
      if not all_match then
        fail "Entries differ after resize";
      Swisstable.len swiss = Collections.HashMap.length hash)

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

let main ~args = Test.Cli.main ~name:"swisstable-model-based-tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
