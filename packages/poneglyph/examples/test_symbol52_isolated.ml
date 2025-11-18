open Std
open Std.UUID
open Poneglyph
open Poneglyph.Storage.Lsm

let () =
  println "\n=== Isolated Symbol52 Test ===\n";
  
  let dir = "/tmp/poneglyph_symbol52_test" in
  (try let _ = Fs.remove_dir_all (Path.v dir) in () with _ -> ());
  ignore (Fs.create_dir_all (Path.v dir));
  
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"create store" in
  
  let source_uri = Uri.of_string "codedb:source:test" in
  let stated_at = Datetime.now () in
  let tx_id = UUID.v7_monotonic () in
  let provided_by_attr = Uri.of_string "codedb:attr:provided_by" in
  
  (* Test Symbol52 *)
  println "=== Testing Symbol52 ===\n";
  let symbol_idx = 52 in
  let file_idx = 52 in
  let file_path = "packages/pkg" ^ string_of_int (file_idx / 10) ^ "/src/file" ^ string_of_int file_idx ^ ".ml" in
  let file_hash = "hash" ^ string_of_int file_idx ^ "abc" in
  let file_entity = Uri.of_string ("codedb:file:" ^ file_path ^ "#" ^ file_hash) in
  let symbol_entity = Uri.of_string ("codedb:symbol:Module" ^ string_of_int symbol_idx ^ ":" ^ file_path) in
  
  let fact = { Fact.fact_uri = Uri.of_string ("fact:symbol" ^ string_of_int symbol_idx ^ "-provided-by");
    source_uri;
    entity = symbol_entity;
    attribute = provided_by_attr;
    value = Fact.Uri file_entity;
    stated_at; tx_id; retracted = false } in
  
  println ("Writing 1 fact:");
  println ("  Entity: " ^ Uri.to_string symbol_entity);
  println ("  Entity SHA-256: " ^ Crypto.Digest.hex (Crypto.Sha256.hash_string (Uri.to_string symbol_entity)));
  println ("  Value: " ^ Uri.to_string file_entity);
  println ("");
  
  (* Write the single fact *)
  let _ = Multi_store.state store [fact]
    |> Result.expect ~msg:"state fact" in
  
  println "✓ Fact written\n";
  
  (* DEBUG: Try to manually check the memtable/engine *)
  println "DEBUG: Checking if we can query by attribute...";
  let provided_by_facts = Multi_store.get_facts_by_attribute store ~attribute:provided_by_attr
    |> Iter.MutIterator.to_list in
  println ("  Facts with provided_by attribute: " ^ string_of_int (List.length provided_by_facts));
  println "";
  
  (* Try reading via get_entity_facts as well *)
  println "Trying Multi_store.get_entity_facts for symbol entity...";
  let get_facts = Multi_store.get_entity_facts store ~entity:symbol_entity
    |> Iter.MutIterator.to_list in
  println ("  get_entity_facts returned " ^ string_of_int (List.length get_facts) ^ " facts\n");
  
  (* Read it back via get_all_current_facts *)
  println "Trying Multi_store.get_all_current_facts...";
  let read_facts = Multi_store.get_all_current_facts store
    |> Iter.MutIterator.to_list in
  
  let count = List.length read_facts in
  println ("Read back " ^ string_of_int count ^ " facts\n");
  
  if count != 1 then begin
    println ("✗ FAIL: Expected 1 fact, got " ^ string_of_int count);
    exit 1
  end;
  
  let read_fact = List.hd read_facts in
  println ("✓ Read back fact:");
  println ("  Entity: " ^ Uri.to_string read_fact.Fact.entity);
  match read_fact.Fact.value with
  | Fact.Uri u ->
      println ("  Value: " ^ Uri.to_string u);
      println "\n✓✓✓ Symbol52 test PASSED! ✓✓✓\n"
  | _ ->
      println "✗ FAIL: Value is not a URI";
      exit 1
