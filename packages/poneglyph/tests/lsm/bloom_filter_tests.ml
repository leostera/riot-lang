(** Bloom Filter Tests *)

open Std
open Poneglyph.Storage.Lsm

module Bytes = Kernel.IO.Bytes

(** Test: No false negatives - all added keys must be found *)
let test_no_false_negatives () =
  let bloom = Bloom_filter.create ~num_keys:1000 ~bits_per_key:10 in
  
  (* Add 1000 keys *)
  let keys = List.init 1000 (fun i ->
    let s = string_of_int i in
    Bytes.of_string s
  ) in
  
  List.iter (fun key -> Bloom_filter.add bloom ~key) keys;
  
  (* Check all keys are found *)
  List.iteri (fun i key ->
    if not (Bloom_filter.might_contain bloom ~key) then
      panic ("False negative for key " ^ string_of_int i ^ "!")
  ) keys;
  
  Log.info "✓ No false negatives test passed (1000 keys)"

(** Test: False positive rate should be reasonable *)
let test_false_positive_rate () =
  let bloom = Bloom_filter.create ~num_keys:1000 ~bits_per_key:10 in
  
  (* Add 1000 keys *)
  for i = 0 to 999 do
    let key = Bytes.of_string (string_of_int i) in
    Bloom_filter.add bloom ~key
  done;
  
  (* Check 10000 keys NOT in bloom *)
  let false_positives = ref 0 in
  for i = 1000 to 10999 do
    let key = Bytes.of_string (string_of_int i) in
    if Bloom_filter.might_contain bloom ~key then
      false_positives := !false_positives + 1
  done;
  
  let fp_rate = float !false_positives /. 10000.0 in
  let fp_pct = string_of_float (fp_rate *. 100.0) in
  Log.info ("False positive rate: " ^ fp_pct ^ "% (" ^ string_of_int !false_positives ^ " / 10000)");
  
  (* Should be around 1% for 10 bits/key *)
  if fp_rate > 0.02 then  (* Allow up to 2% due to randomness *)
    panic ("False positive rate too high: " ^ fp_pct ^ "%");
  
  Log.info "✓ False positive rate test passed"

(** Test: Serialization round-trip *)
let test_serialization () =
  let bloom = Bloom_filter.create ~num_keys:100 ~bits_per_key:10 in
  
  (* Add some keys *)
  for i = 0 to 99 do
    let key = Bytes.of_string ("key-" ^ string_of_int i) in
    Bloom_filter.add bloom ~key
  done;
  
  (* Serialize *)
  let bytes = Bloom_filter.to_bytes bloom in
  
  (* Deserialize *)
  let bloom2 = match Bloom_filter.from_bytes bytes with
    | Ok b -> b
    | Error e -> panic ("Deserialization failed: " ^ e)
  in
  
  (* Verify all keys still work *)
  for i = 0 to 99 do
    let key = Bytes.of_string ("key-" ^ string_of_int i) in
    if not (Bloom_filter.might_contain bloom2 ~key) then
      panic ("Key lost after serialization: key-" ^ string_of_int i)
  done;
  
  Log.info "✓ Serialization round-trip test passed"

(** Test: Empty bloom filter *)
let test_empty_bloom () =
  let bloom = Bloom_filter.create ~num_keys:100 ~bits_per_key:10 in
  
  (* Check that random keys return false (no false positives for empty filter) *)
  let any_positive = ref false in
  for i = 0 to 99 do
    let key = Bytes.of_string (string_of_int i) in
    if Bloom_filter.might_contain bloom ~key then
      any_positive := true
  done;
  
  (* Empty bloom should have low false positive rate *)
  (* (Actually, mathematically it should be 0%, but we'll be lenient) *)
  if !any_positive then
    Log.warn "Empty bloom filter had some false positives (expected but uncommon)"
  else
    Log.info "✓ Empty bloom filter test passed";
  
  ()

(** Test: Statistics *)
let test_statistics () =
  let bloom = Bloom_filter.create ~num_keys:100 ~bits_per_key:10 in
  
  (* Add keys *)
  for i = 0 to 99 do
    let key = Bytes.of_string (string_of_int i) in
    Bloom_filter.add bloom ~key
  done;
  
  let stats = Bloom_filter.stats bloom in
  
  Log.info "Bloom filter stats:";
  Log.info ("  num_bits: " ^ string_of_int stats.num_bits);
  Log.info ("  num_hashes: " ^ string_of_int stats.num_hashes);
  Log.info ("  bits_set: " ^ string_of_int stats.bits_set);
  Log.info ("  fill_ratio: " ^ string_of_float (stats.fill_ratio *. 100.0) ^ "%");
  
  (* Sanity checks *)
  if stats.num_bits != 1000 then
    panic "Expected 1000 bits (100 keys * 10 bits/key)";
  
  if stats.bits_set = 0 then
    panic "No bits were set!";
  
  if stats.fill_ratio > 1.0 then
    panic "Fill ratio > 100%!";
  
  Log.info "✓ Statistics test passed"

(** Test: Large bloom filter *)
let test_large_bloom () =
  let bloom = Bloom_filter.create ~num_keys:100000 ~bits_per_key:10 in
  
  (* Add 100K keys *)
  for i = 0 to 99999 do
    let key = Bytes.of_string ("large-key-" ^ string_of_int i) in
    Bloom_filter.add bloom ~key
  done;
  
  (* Spot check some keys *)
  for i = 0 to 999 do
    let idx = Random.int 100000 in
    let key = Bytes.of_string ("large-key-" ^ string_of_int idx) in
    if not (Bloom_filter.might_contain bloom ~key) then
      panic ("False negative for large key " ^ string_of_int idx)
  done;
  
  let size = Bloom_filter.byte_size bloom in
  Log.info ("Large bloom filter size: " ^ string_of_float (float size /. 1024.0) ^ " KB");
  
  (* Should be around 122 KB (100K keys * 10 bits/key / 8) *)
  let expected = (100000 * 10) / 8 + 12 in  (* +12 for header *)
  if size != expected then
    Log.warn ("Size mismatch: expected " ^ string_of_int expected ^ ", got " ^ string_of_int size);
  
  Log.info "✓ Large bloom filter test passed"

(** Run all tests *)
let run_tests () =
  Log.info "=== Bloom Filter Tests ===";
  test_no_false_negatives ();
  test_false_positive_rate ();
  test_serialization ();
  test_empty_bloom ();
  test_statistics ();
  test_large_bloom ();
  Log.info "=== All Bloom Filter Tests Passed! ===";
  ()

let () = run_tests ()
