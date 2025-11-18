(** Test manifest save/load roundtrip *)

open Std

let test_tier_for_size () =
  let open Poneglyph.Storage.Lsm.Manifest in
  assert (tier_for_size 500_000 = 0);      (* < 1MB = tier 0 *)
  assert (tier_for_size 2_000_000 = 1);    (* 1-8MB = tier 1 *)
  assert (tier_for_size 20_000_000 = 2);   (* 8-64MB = tier 2 *)
  assert (tier_for_size 100_000_000 = 3);  (* 64-512MB = tier 3 *)
  println "✓ test_tier_for_size passed"

let test_manifest_roundtrip () =
  let open Poneglyph.Storage.Lsm.Manifest in
  let manifest = empty () in
  let meta = {
    path = "sstable_1.sst";
    tier = 0;
    size_bytes = 491;
    min_key = String.to_bytes "aaa";
    max_key = String.to_bytes "zzz";
    entry_count = 1;
    created_at = 123456789L;
  } in
  
  let manifest' = add_sstable manifest ~index:"test" meta in
  
  (* Save *)
  let path = "/tmp/test_manifest_" ^ (UUID.v4 () |> UUID.to_string) ^ ".json" in
  save ~path manifest' |> Result.expect ~msg:"save failed";
  
  (* Load *)
  let loaded = load ~path |> Result.expect ~msg:"load failed" in
  
  (* Verify *)
  let sstables = get_sstables loaded ~index:"test" in
  assert (List.length sstables = 1);
  
  let loaded_meta = List.hd sstables in
  assert (loaded_meta.path = "sstable_1.sst");
  assert (loaded_meta.tier = 0);
  assert (loaded_meta.size_bytes = 491);
  assert (loaded_meta.entry_count = 1);
  assert (loaded_meta.created_at = 123456789L);
  assert (loaded_meta.min_key = String.to_bytes "aaa");
  assert (loaded_meta.max_key = String.to_bytes "zzz");
  
  (* Cleanup *)
  Fs.remove_file (Path.v path) |> ignore;
  
  println "✓ test_manifest_roundtrip passed"

let test_multiple_indices () =
  let open Poneglyph.Storage.Lsm.Manifest in
  let manifest = empty () in
  
  let meta1 = {
    path = "eavt_1.sst";
    tier = 0;
    size_bytes = 491;
    min_key = String.to_bytes "e1";
    max_key = String.to_bytes "e2";
    entry_count = 1;
    created_at = 100L;
  } in
  
  let meta2 = {
    path = "avet_1.sst";
    tier = 1;
    size_bytes = 2_000_000;
    min_key = String.to_bytes "a1";
    max_key = String.to_bytes "a2";
    entry_count = 100;
    created_at = 200L;
  } in
  
  let manifest' = add_sstable manifest ~index:"eavt" meta1 in
  let manifest'' = add_sstable manifest' ~index:"avet" meta2 in
  
  (* Verify eavt *)
  let eavt_sstables = get_sstables manifest'' ~index:"eavt" in
  assert (List.length eavt_sstables = 1);
  assert ((List.hd eavt_sstables).path = "eavt_1.sst");
  
  (* Verify avet *)
  let avet_sstables = get_sstables manifest'' ~index:"avet" in
  assert (List.length avet_sstables = 1);
  assert ((List.hd avet_sstables).path = "avet_1.sst");
  assert ((List.hd avet_sstables).tier = 1);
  
  println "✓ test_multiple_indices passed"

let test_remove_sstables () =
  let open Poneglyph.Storage.Lsm.Manifest in
  let manifest = empty () in
  
  (* Add 3 SSTables *)
  let meta1 = {
    path = "sst_1.sst";
    tier = 0;
    size_bytes = 491;
    min_key = String.to_bytes "a";
    max_key = String.to_bytes "b";
    entry_count = 1;
    created_at = 100L;
  } in
  let meta2 = { meta1 with path = "sst_2.sst" } in
  let meta3 = { meta1 with path = "sst_3.sst" } in
  
  let manifest' = add_sstable manifest ~index:"test" meta1 in
  let manifest'' = add_sstable manifest' ~index:"test" meta2 in
  let manifest''' = add_sstable manifest'' ~index:"test" meta3 in
  
  assert (List.length (get_sstables manifest''' ~index:"test") = 3);
  
  (* Remove 2 SSTables *)
  let manifest_final = remove_sstables manifest''' ~index:"test" 
    ~paths:["sst_1.sst"; "sst_2.sst"] in
  
  let remaining = get_sstables manifest_final ~index:"test" in
  assert (List.length remaining = 1);
  assert ((List.hd remaining).path = "sst_3.sst");
  
  println "✓ test_remove_sstables passed"

let test_group_by_tier () =
  let open Poneglyph.Storage.Lsm.Manifest in
  let manifest = empty () in
  
  (* Add SSTables to different tiers *)
  let make_meta path tier = {
    path;
    tier;
    size_bytes = 491;
    min_key = String.to_bytes "a";
    max_key = String.to_bytes "b";
    entry_count = 1;
    created_at = 100L;
  } in
  
  let manifest' = add_sstable manifest ~index:"test" (make_meta "t0_1.sst" 0) in
  let manifest'' = add_sstable manifest' ~index:"test" (make_meta "t0_2.sst" 0) in
  let manifest''' = add_sstable manifest'' ~index:"test" (make_meta "t1_1.sst" 1) in
  
  let sstables = get_sstables manifest''' ~index:"test" in
  let by_tier = group_by_tier sstables in
  
  (* Should have tier 0 and tier 1 *)
  assert (List.length by_tier = 2);
  
  (* Tier 0 should have 2 SSTables *)
  let tier0 = List.assoc 0 by_tier in
  assert (List.length tier0 = 2);
  
  (* Tier 1 should have 1 SSTable *)
  let tier1 = List.assoc 1 by_tier in
  assert (List.length tier1 = 1);
  
  println "✓ test_group_by_tier passed"

let run () =
  println "Running manifest tests...";
  test_tier_for_size ();
  test_manifest_roundtrip ();
  test_multiple_indices ();
  test_remove_sstables ();
  test_group_by_tier ();
  println "\n✓ All manifest tests passed!"

let () = run ()
