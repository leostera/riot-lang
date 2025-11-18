open Std
open Std.UUID
open Poneglyph
open Poneglyph.Storage.Lsm

let setup_test_dir () =
  let test_dir = "/tmp/poneglyph_test_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

(* Generate file facts similar to codedb *)
let generate_file_facts ~batch_num ~files_per_batch ~tx_id ~source_uri ~stated_at =
  let path_attr = Uri.of_string "codedb:attr:path" in
  let sha256_attr = Uri.of_string "codedb:attr:sha256" in
  
  let files = List.init files_per_batch (fun i ->
    let file_idx = batch_num * files_per_batch + i in
    let file_path = "packages/pkg" ^ string_of_int (file_idx / 10) ^ "/src/file" ^ string_of_int file_idx ^ ".ml" in
    let file_hash = "hash" ^ string_of_int file_idx ^ "abc" in
    let file_entity = Uri.of_string ("codedb:file:" ^ file_path ^ "#" ^ file_hash) in
    
    (* Each file creates 2 facts: path and sha256 *)
    [
      { Fact.fact_uri = Uri.of_string ("fact:file" ^ string_of_int file_idx ^ "-path");
        source_uri;
        entity = file_entity;
        attribute = path_attr;
        value = Fact.String file_path;
        stated_at; tx_id; retracted = false };
      
      { Fact.fact_uri = Uri.of_string ("fact:file" ^ string_of_int file_idx ^ "-hash");
        source_uri;
        entity = file_entity;
        attribute = sha256_attr;
        value = Fact.String file_hash;
        stated_at; tx_id; retracted = false };
    ]
  ) in
  List.concat files

(* Generate symbol facts that reference files via URI *)
let generate_symbol_facts ~batch_num ~symbols_per_batch ~tx_id ~source_uri ~stated_at =
  let provided_by_attr = Uri.of_string "codedb:attr:provided_by" in
  
  let symbols = List.init symbols_per_batch (fun i ->
    let symbol_idx = batch_num * symbols_per_batch + i in
    let file_idx = symbol_idx in (* Symbol references corresponding file *)
    let file_path = "packages/pkg" ^ string_of_int (file_idx / 10) ^ "/src/file" ^ string_of_int file_idx ^ ".ml" in
    let file_hash = "hash" ^ string_of_int file_idx ^ "abc" in
    let file_entity = Uri.of_string ("codedb:file:" ^ file_path ^ "#" ^ file_hash) in
    
    let symbol_entity = Uri.of_string ("codedb:symbol:Module" ^ string_of_int symbol_idx ^ ":" ^ file_path) in
    
    (* Symbol has 1 fact: provided_by pointing to file *)
    { Fact.fact_uri = Uri.of_string ("fact:symbol" ^ string_of_int symbol_idx ^ "-provided-by");
      source_uri;
      entity = symbol_entity;
      attribute = provided_by_attr;
      value = Fact.Uri file_entity;  (* URI reference! *)
      stated_at; tx_id; retracted = false }
  ) in
  symbols

let () =
  Random.init 42;
  println "\n=== Batched URI Value Test (1000+ facts) ===\n";
  
  let dir = setup_test_dir () in
  cleanup_test_dir dir;  (* Clean up any previous run *)
  ignore (Fs.create_dir_all (Path.v dir));
  
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"create store" in
  
  let source_uri = Uri.of_string "codedb:source:test" in
  let stated_at = Datetime.now () in
  
  (* Configuration *)
  let batch_size = 50 in  (* 50 files per batch *)
  let num_batches = 4 in  (* 4 batches = 200 files *)
  let total_files = batch_size * num_batches in
  
  println ("Configuration:");
  println ("  Batches: " ^ string_of_int num_batches);
  println ("  Files per batch: " ^ string_of_int batch_size);
  println ("  Total files: " ^ string_of_int total_files);
  println ("  Facts per file: 2 (path + sha256)");
  println ("  Symbols per batch: " ^ string_of_int batch_size);
  println ("  Total facts: ~" ^ string_of_int (total_files * 3) ^ " (files + symbols)\n");
  
  (* Write facts in batches *)
  println "=== Writing facts in batches ===\n";
  
  let total_facts_written = ref 0 in
  
  for batch_num = 0 to num_batches - 1 do
    let tx_id = UUID.v7_monotonic () in
    
    (* Generate file facts (2 per file) *)
    let file_facts = generate_file_facts 
      ~batch_num 
      ~files_per_batch:batch_size 
      ~tx_id 
      ~source_uri 
      ~stated_at in
    
    (* Generate symbol facts (1 per file, with URI reference) *)
    let symbol_facts = generate_symbol_facts
      ~batch_num
      ~symbols_per_batch:batch_size
      ~tx_id
      ~source_uri
      ~stated_at in
    
    let batch_facts = file_facts @ symbol_facts in
    let batch_fact_count = List.length batch_facts in
    
    println ("Batch " ^ string_of_int (batch_num + 1) ^ "/" ^ string_of_int num_batches ^ 
             ": writing " ^ string_of_int batch_fact_count ^ " facts...");
    
    (* DEBUG: Check if symbol52 is in batch 2 *)
    if batch_num = 1 then begin
      let symbol52_uri = "codedb:symbol:Module52:packages/pkg5/src/file52.ml" in
      let has_symbol52 = List.exists (fun f ->
        Uri.to_string f.Fact.entity = symbol52_uri
      ) batch_facts in
      println ("  DEBUG: Batch 2 contains Module52? " ^ string_of_bool has_symbol52);
      if has_symbol52 then
        println ("  DEBUG: Symbol52 fact WAS generated correctly")
    end;
    
    let _ = Multi_store.state store batch_facts
      |> Result.expect ~msg:("state batch " ^ string_of_int batch_num) in
    
    total_facts_written := !total_facts_written + batch_fact_count;
    
    (* Verify facts can be read back immediately after writing *)
    let read_back_count = Multi_store.get_all_current_facts store
      |> Iter.MutIterator.count in
    let expected_so_far = !total_facts_written in
    
    println ("  ✓ Batch " ^ string_of_int (batch_num + 1) ^ " written");
    println ("    Written so far: " ^ string_of_int expected_so_far);
    println ("    Read back: " ^ string_of_int read_back_count);
    
    if read_back_count != expected_so_far then
      println ("    ⚠️  MISMATCH! Missing " ^ string_of_int (expected_so_far - read_back_count) ^ " facts!");
    println "";
  done;
  
  println ("✓ All batches written successfully");
  println ("  Total facts written: " ^ string_of_int !total_facts_written ^ "\n");
  
  (* Read it back using get_all_current_facts (this is what stats uses) *)
  println "=== Reading back all facts (get_all_current_facts) ===";
  let all_facts = Multi_store.get_all_current_facts store
    |> Iter.MutIterator.to_list in
  
  let retrieved_count = List.length all_facts in
  println ("Retrieved " ^ string_of_int retrieved_count ^ " facts\n");
  
  if retrieved_count != !total_facts_written then begin
    println ("⚠️  Expected " ^ string_of_int !total_facts_written ^ " facts, got " ^ string_of_int retrieved_count);
    println "This is the bug we're looking for! Continuing to analyze...";
  end;
  
  println "=== Verifying all facts decoded correctly ===";
  
  (* Count fact types *)
  let uri_fact_count = ref 0 in
  let string_fact_count = ref 0 in
  let uri_lookup_errors = ref 0 in
  
  List.iter (fun fact ->
    match fact.Fact.value with
    | Fact.String _ ->
        string_fact_count := !string_fact_count + 1
    | Fact.Uri u ->
        uri_fact_count := !uri_fact_count + 1;
        (* Verify the URI is valid *)
        let uri_str = Uri.to_string u in
        if not (String.starts_with ~prefix:"codedb:file:" uri_str) then (
          println ("✗ Unexpected URI reference: " ^ uri_str);
          uri_lookup_errors := !uri_lookup_errors + 1
        )
    | _ ->
        panic "Unexpected value type"
  ) all_facts;
  
  println ("✓ All " ^ string_of_int retrieved_count ^ " facts decoded successfully");
  println ("  String-valued facts: " ^ string_of_int !string_fact_count);
  println ("  URI-valued facts: " ^ string_of_int !uri_fact_count);
  
  if !uri_lookup_errors > 0 then
    panic ("Found " ^ string_of_int !uri_lookup_errors ^ " invalid URI references");
  
  (* Expected: 200 files × 2 facts (path + sha256) = 400 string facts *)
  (*           200 symbols × 1 fact (provided_by) = 200 URI facts *)
  let expected_string_facts = batch_size * num_batches * 2 in
  let expected_uri_facts = batch_size * num_batches in
  
  if !string_fact_count != expected_string_facts then
    panic ("Expected " ^ string_of_int expected_string_facts ^ " string facts, got " ^ string_of_int !string_fact_count);
  
  if !uri_fact_count != expected_uri_facts then begin
    println ("✗ Expected " ^ string_of_int expected_uri_facts ^ " URI facts, got " ^ string_of_int !uri_fact_count);
    
    (* Find which symbol is missing *)
    println "\n=== Finding missing symbol ===";
    let found_symbols = ref [] in
    List.iter (fun fact ->
      match fact.Fact.value with
      | Fact.Uri _ ->
          let uri_str = Uri.to_string fact.Fact.entity in
          if String.starts_with ~prefix:"codedb:symbol:" uri_str then
            found_symbols := uri_str :: !found_symbols
      | _ -> ()
    ) all_facts;
    
    (* Check which symbols are missing *)
    for i = 0 to expected_uri_facts - 1 do
      let file_path = "packages/pkg" ^ string_of_int (i / 10) ^ "/src/file" ^ string_of_int i ^ ".ml" in
      let symbol_uri_str = "codedb:symbol:Module" ^ string_of_int i ^ ":" ^ file_path in
      if not (List.mem symbol_uri_str !found_symbols) then begin
        println ("✗ MISSING: " ^ symbol_uri_str);
        
        (* Check for hash collision *)
        let file_hash = "hash" ^ string_of_int i ^ "abc" in
        let file_entity_str = "codedb:file:" ^ file_path ^ "#" ^ file_hash in
        let file_entity_sha = Crypto.Digest.bytes (Crypto.Sha256.hash_string file_entity_str) in
        let symbol_entity_sha = Crypto.Digest.bytes (Crypto.Sha256.hash_string symbol_uri_str) in
        
        let file_hex = Crypto.Digest.hex (Crypto.Sha256.hash_string file_entity_str) in
        let symbol_hex = Crypto.Digest.hex (Crypto.Sha256.hash_string symbol_uri_str) in
        
        println ("  File entity:   " ^ file_entity_str);
        println ("    SHA-256: " ^ file_hex);
        println ("  Symbol entity: " ^ symbol_uri_str);
        println ("    SHA-256: " ^ symbol_hex);
        
        (* Check first 16 hex chars (8 bytes) *)
        let file_first16 = String.sub file_hex 0 16 in
        let symbol_first16 = String.sub symbol_hex 0 16 in
        if file_first16 = symbol_first16 then
          println ("  ⚠️  HASH COLLISION! First 8 bytes match!")
      end
    done;
    
    panic ("Expected " ^ string_of_int expected_uri_facts ^ " URI facts, got " ^ string_of_int !uri_fact_count)
  end;
  
  let _ = Multi_store.close store in
  cleanup_test_dir dir;
  println "\n✓✓✓ Test passed! ✓✓✓\n"
