(** Property-Based Tests for SwissTable HashMap
    
    Uses Propane for property-based testing with random generation and shrinking.
    Focuses on integer keys for hash stability across runs.
*)

open Std
open Propane

(** {1 Custom Generators and Arbitraries} *)

(* Small integers for better collision testing *)
let small_int = Generator.int_range 0 50

(* Key-value pair generator *)
let kv_pair = Generator.pair small_int small_int

(* Generate a populated SwissTable *)
let swisstable_gen key_gen value_gen =
  Generator.map
    (fun pairs ->
      let map = Swisstable.create () in
      List.iter (fun (k, v) ->
        let _ = Swisstable.insert map k v in ()
      ) pairs;
      map)
    (Generator.list (Generator.pair key_gen value_gen))

(* Arbitrary for populated SwissTable maps *)
let swisstable key_arb value_arb =
  Arbitrary.make
    ~print:(fun map ->
      let entries = Swisstable.to_list map in
      let pairs_str = String.concat ", " (List.map (fun (k, v) ->
        let k_str = match key_arb.Arbitrary.print with
          | Some p -> p k
          | None -> "?"
        in
        let v_str = match value_arb.Arbitrary.print with
          | Some p -> p v
          | None -> "?"
        in
        k_str ^ " -> " ^ v_str
      ) entries) in
      "{" ^ pairs_str ^ "}")
    (swisstable_gen key_arb.Arbitrary.gen value_arb.Arbitrary.gen)

(* Convenience: populated map with int keys and values *)
let populated_map = swisstable Arbitrary.int Arbitrary.int

(** {1 Basic Properties} *)

(* Property 1: Insert-Get Round-trip *)
let insert_get_prop =
  property "insert then get returns the value"
    Arbitrary.(triple int int populated_map)
    (fun (key, value, map) ->
      let _ = Swisstable.insert map key value in
      match Swisstable.get map key with
      | Some v -> v = value
      | None -> fail "key not found after insert")

(* Property 2: Remove-Get Consistency *)
let remove_get_prop =
  property "get returns None after remove"
    Arbitrary.(pair int populated_map)
    (fun (key, map) ->
      (* First insert to ensure key exists *)
      let _ = Swisstable.insert map key 42 in
      (* Then remove *)
      let _ = Swisstable.remove map key in
      (* Verify it's gone *)
      Swisstable.get map key = None)

(* Property 3: Contains-Get Equivalence *)
let contains_get_equiv_prop =
  property "contains_key equivalent to is_some(get)"
    Arbitrary.(pair int populated_map)
    (fun (key, map) ->
      let has_key = Swisstable.contains_key map key in
      let get_result = Swisstable.get map key in
      has_key = Option.is_some get_result)

(* Property 4: Insert is idempotent for same key-value *)
let insert_idempotent_prop =
  property "inserting same (k,v) twice is idempotent"
    Arbitrary.(triple int int populated_map)
    (fun (key, value, map) ->
      let _ = Swisstable.insert map key value in
      let len1 = Swisstable.len map in
      let _ = Swisstable.insert map key value in
      let len2 = Swisstable.len map in
      len1 = len2)

(* Property 5: Remove absent key is no-op *)
let remove_absent_noop_prop =
  property "removing absent key is no-op"
    Arbitrary.(pair int populated_map)
    (fun (key, map) ->
      (* Ensure key doesn't exist *)
      let _ = Swisstable.remove map key in
      let len1 = Swisstable.len map in
      let result = Swisstable.remove map key in
      let len2 = Swisstable.len map in
      result = None && len1 = len2)

(* Property 6: Length after insert *)
let length_after_insert_prop =
  property "length increases by at most 1 after insert"
    Arbitrary.(triple int int populated_map)
    (fun (key, value, map) ->
      let len_before = Swisstable.len map in
      let _ = Swisstable.insert map key value in
      let len_after = Swisstable.len map in
      len_after = len_before || len_after = len_before + 1)

(* Property 7: Length after remove *)
let length_after_remove_prop =
  property "length decreases by at most 1 after remove"
    Arbitrary.(pair int populated_map)
    (fun (key, map) ->
      let len_before = Swisstable.len map in
      let _ = Swisstable.remove map key in
      let len_after = Swisstable.len map in
      len_after = len_before || len_after = len_before - 1)

(** {1 Iteration Properties} *)

(* Property 8: to_list length equals len *)
let to_list_length_prop =
  property "to_list length equals len"
    populated_map
    (fun map ->
      let lst = Swisstable.to_list map in
      Collections.List.length lst = Swisstable.len map)

(* Property 9: keys length equals len *)
let keys_length_prop =
  property "keys length equals len"
    populated_map
    (fun map ->
      let keys = Swisstable.keys map in
      Collections.List.length keys = Swisstable.len map)

(* Property 10: values length equals len *)
let values_length_prop =
  property "values length equals len"
    populated_map
    (fun map ->
      let values = Swisstable.values map in
      Collections.List.length values = Swisstable.len map)

(* Property 11: All entries in to_list are gettable *)
let to_list_entries_gettable_prop =
  property "all entries in to_list are gettable"
    populated_map
    (fun map ->
      let entries = Swisstable.to_list map in
      List.for_all (fun (k, v) ->
        match Swisstable.get map k with
        | Some v' -> v = v'
        | None -> false
      ) entries)

(* Property 12: Fold count equals len *)
let fold_count_prop =
  property "fold counting equals len"
    populated_map
    (fun map ->
      let count = Swisstable.fold (fun _ _ acc -> acc + 1) map 0 in
      count = Swisstable.len map)

(** {1 Entry API Properties} *)

(* Property 13: or_insert on vacant key creates entry *)
let or_insert_vacant_prop =
  property "or_insert creates entry for vacant key"
    Arbitrary.(triple int int populated_map)
    (fun (key, value, map) ->
      (* Ensure key doesn't exist *)
      let _ = Swisstable.remove map key in
      let len_before = Swisstable.len map in
      let result = Swisstable.or_insert map key value in
      let len_after = Swisstable.len map in
      result = value && len_after = len_before + 1)

(* Property 14: or_insert on occupied key returns existing *)
let or_insert_occupied_prop =
  property "or_insert returns existing value for occupied key"
    Arbitrary.(triple int int populated_map)
    (fun (key, old_value, map) ->
      (* Insert old value first *)
      let _ = Swisstable.insert map key old_value in
      let len_before = Swisstable.len map in
      (* Try to insert new value *)
      let result = Swisstable.or_insert map key 9999 in
      let len_after = Swisstable.len map in
      result = old_value && len_before = len_after)

(* Property 15: and_modify only affects existing keys *)
let and_modify_existing_prop =
  property "and_modify only modifies existing keys"
    Arbitrary.(pair int populated_map)
    (fun (key, map) ->
      (* Try to modify non-existent key *)
      let _ = Swisstable.remove map key in
      Swisstable.and_modify map key (fun x -> x + 1);
      Swisstable.get map key = None)

(** {1 Clear Properties} *)

(* Property 16: Clear makes map empty *)
let clear_makes_empty_prop =
  property "clear makes map empty"
    populated_map
    (fun map ->
      Swisstable.clear map;
      Swisstable.len map = 0 && Swisstable.is_empty map)

(* Property 17: After clear, all gets return None *)
let clear_all_none_prop =
  property "after clear, all gets return None"
    populated_map
    (fun map ->
      let keys = Swisstable.keys map in
      Swisstable.clear map;
      List.for_all (fun k -> Swisstable.get map k = None) keys)

(** {1 Resize Properties} *)

(* Property 18: Many insertions preserve all entries *)
let many_insertions_prop =
  property "many insertions preserve all entries"
    (Arbitrary.list (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 100);  (* Limit test size *)
      let map = Swisstable.create () in
      
      (* Insert all pairs *)
      List.iter (fun (k, v) ->
        let _ = Swisstable.insert map k v in ()
      ) pairs;
      
      (* Build reference map using HashMap to deduplicate *)
      let ref_map = Collections.HashMap.create () in
      List.iter (fun (k, v) ->
        Collections.HashMap.insert ref_map k v |> ignore
      ) pairs;
      
      (* Verify all unique keys are accessible and match reference *)
      Collections.HashMap.iter (fun k expected_v ->
        match Swisstable.get map k with
        | Some actual_v -> 
            if not (actual_v = expected_v) then
              fail "Value mismatch after many insertions"
        | None -> fail "Key missing after many insertions"
      ) ref_map;
      
      (* Also check lengths match *)
      Swisstable.len map = Collections.HashMap.len ref_map)

(* Property 19: Length is correct after many operations *)
let length_invariant_prop =
  property "length invariant holds across operations"
    (Arbitrary.list (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let map = Swisstable.create () in
      
      (* Count unique keys using reference HashMap *)
      let ref_map = Collections.HashMap.create () in
      List.iter (fun (k, v) ->
        Collections.HashMap.insert ref_map k v |> ignore
      ) pairs;
      
      (* Insert all into swisstable *)
      List.iter (fun (k, v) ->
        let _ = Swisstable.insert map k v in ()
      ) pairs;
      
      (* Length should equal unique keys *)
      Swisstable.len map = Collections.HashMap.len ref_map)

(** {1 Overwrite Properties} *)

(* Property 20: Insert returns previous value *)
let insert_returns_previous_prop =
  property "insert returns previous value if key exists"
    Arbitrary.(triple int int int)
    (fun (key, old_val, new_val) ->
      let map = Swisstable.create () in
      let result1 = Swisstable.insert map key old_val in
      let result2 = Swisstable.insert map key new_val in
      result1 = None && result2 = Some old_val)

(* Property 21: Remove returns removed value *)
let remove_returns_value_prop =
  property "remove returns the removed value"
    Arbitrary.(pair int int)
    (fun (key, value) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map key value in
      let result = Swisstable.remove map key in
      result = Some value)

(** {1 Empty Map Properties} *)

(* Property 22: Empty map has length 0 *)
let empty_map_length_prop =
  property "newly created map has length 0"
    Arbitrary.int  (* Dummy input *)
    (fun _ ->
      let map = Swisstable.create () in
      Swisstable.len map = 0 && Swisstable.is_empty map)

(* Property 23: Get on empty map returns None *)
let empty_map_get_prop =
  property "get on empty map returns None"
    Arbitrary.int
    (fun key ->
      let map = Swisstable.create () in
      Swisstable.get map key = None)

(** {1 Test Suite} *)

let tests = [
  (* Basic operations *)
  insert_get_prop;
  remove_get_prop;
  contains_get_equiv_prop;
  insert_idempotent_prop;
  remove_absent_noop_prop;
  length_after_insert_prop;
  length_after_remove_prop;
  
  (* Iteration *)
  to_list_length_prop;
  keys_length_prop;
  values_length_prop;
  to_list_entries_gettable_prop;
  fold_count_prop;
  
  (* Entry API *)
  or_insert_vacant_prop;
  or_insert_occupied_prop;
  and_modify_existing_prop;
  
  (* Clear *)
  clear_makes_empty_prop;
  clear_all_none_prop;
  
  (* Resize *)
  many_insertions_prop;
  length_invariant_prop;
  
  (* Overwrite *)
  insert_returns_previous_prop;
  remove_returns_value_prop;
  
  (* Empty *)
  empty_map_length_prop;
  empty_map_get_prop;
]

let () =
  Miniriot.run 
    ~main:(fun ~args -> Test.Cli.main ~name:"swisstable-property-tests" ~tests ~args) 
    ~args:Env.args ()
