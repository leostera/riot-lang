open Std
open Std.Collections

(** Scan a directory for .ml files *)
let rec scan_directory dir =
  let files = ref [] in
  
  let rec scan path =
    match Fs.read_dir path with
    | Error _ -> ()
    | Ok iter ->
        Iter.MutIterator.for_each ~fn:(fun entry ->
          let name = Path.to_string entry in
          let full_path = Path.(path / entry) in
          
          (* Skip hidden files/dirs and build artifacts *)
          if String.starts_with ~prefix:"." name || name = "_build" then ()
          else if String.ends_with ~suffix:".ml" name then
            files := full_path :: !files
          else
            (* Assume it's a directory if no extension *)
            if not (String.contains name ".") then
              scan full_path
        ) iter
  in
  
  scan dir;
  !files

(** Helper: convert file to module symbol if it's a .ml file in packages/ *)
let file_to_module_symbol (file : Codedb.Model.File.t) : Codedb.Model.Symbol.t option =
  let path_str = Path.to_string file.path in
  
  (* Only .ml files *)
  if not (String.ends_with ~suffix:".ml" path_str) then None
  (* Skip tests and examples *)
  else
    let namespace = Codedb.Model.Namespace.from_path file.path in
    if Codedb.Model.Namespace.is_empty namespace then None
    else
      (* Extract package name from path *)
      let pkg_name = match String.split_on_char '/' path_str with
        | "packages" :: pkg :: _ -> pkg
        | _ -> "unknown"
      in
      
      (* Simple module name from filename *)
      let simple_name = 
        file.path
        |> Path.remove_extension 
        |> Path.basename 
        |> String.capitalize_ascii
      in
      
      (* Create module_name *)
      let module_name = Codedb.Model.Module_name.make
        ~filename:file.path
        ~namespace
        ~name:simple_name
      in
      
      (* Create package_info *)
      let package_info = Codedb.Model.Package_info.make
        ~name:(Codedb.Model.Package_name.of_string_exn pkg_name)
        ~path:Path.(v "packages/" / v path_str)
      in
      
      (* Create files record based on extension *)
      let files = 
        match Path.extension file.path with
        | Some ".mli" -> Codedb.Model.Symbol.{ implementation = None; interface = Some file }
        | _ -> Codedb.Model.Symbol.{ implementation = Some file; interface = None }
      in
      
      Some (Codedb.Model.Symbol.make
        ~kind:Codedb.Model.Symbol.Module
        ~name:module_name
        ~package:package_info
        ~files)

(** Create File entities from paths *)
let create_file_entities paths =
  List.filter_map (fun path ->
    match Fs.read_to_string path with
    | Ok content ->
        let hash = Crypto.Sha256.hash_string content in
        let sha256 = Crypto.Digest.hex hash in
        Some (Codedb.Model.File.make 
          ~path 
          ~sha256 
          ~size:(String.length content) 
          ())
    | Error _ -> None
  ) paths

(** State files in batches *)
let state_files_in_batches graph files batch_size =
  let total = List.length files in
  let num_batches = (total + batch_size - 1) / batch_size in
  
  println (String.concat "" [
    "Stating "; string_of_int total; " files in "; 
    string_of_int num_batches; " batches of "; string_of_int batch_size; "..."
  ]);
  
  let start_time = Time.Instant.now () in
  let tx_id = UUID.v7_monotonic () in
  
  let rec process_batches batch_num remaining =
    match remaining with
    | [] -> ()
    | _ ->
        let batch, rest = 
          let rec take n acc = function
            | [] -> (List.rev acc, [])
            | xs when n = 0 -> (List.rev acc, xs)
            | x :: xs -> take (n - 1) (x :: acc) xs
          in
          take batch_size [] remaining
        in
        
        let batch_facts = List.concat_map (fun file ->
          match file_to_module_symbol file with
          | None -> Codedb.Model.File.to_facts ~tx_id file
          | Some symbol -> Codedb.Model.Symbol.to_facts ~tx_id symbol
        ) batch in
        
        let _ = Poneglyph.state graph batch_facts in
        
        println (String.concat "" [
          "  Batch "; string_of_int (batch_num + 1); "/"; string_of_int num_batches;
          ": stated "; string_of_int (List.length batch_facts); " facts (";
          string_of_int (List.length batch); " files)"
        ]);
        
        process_batches (batch_num + 1) rest
  in
  
  process_batches 0 files;
  
  let elapsed = Time.Instant.elapsed start_time in
  let elapsed_ms = Time.Duration.to_millis elapsed in
  let files_per_sec = Float.of_int total /. (Float.of_int elapsed_ms /. 1000.0) in
  println (String.concat "" [
    "Total time: "; string_of_int elapsed_ms; "ms (";
    Float.to_string files_per_sec; " files/sec)"
  ]);
  
  elapsed

(** Run a query and measure time *)
let timed_query name f =
  let start = Time.Instant.now () in
  let result = f () in
  let elapsed = Time.Instant.elapsed start in
  let elapsed_ms = Time.Duration.to_millis elapsed in
  println (String.concat "" ["\n["; name; "] took "; string_of_int elapsed_ms; "ms"]);
  (result, elapsed_ms)

let main () =
  Std.Log.(set_level Error);
  println "=== CodeDB Performance Demo ===\n";
  
  
  (* 1. Open database *)
  println "1. Opening database...";
  let graph = match Poneglyph.open_exclusive ~data_dir:".codedb.pone" () with
    | Error e -> panic ("Failed to open database: " ^ e)
    | Ok g -> g
  in
  println "   ✓ Database opened\n";
  
  (* 2. Scan codebase *)
  println "2. Scanning codebase for .ml files...";
  let scan_start = Time.Instant.now () in
  let files = scan_directory (Path.v "packages") in
  let scan_elapsed = Time.Instant.elapsed scan_start |> Time.Duration.to_millis in
  println (String.concat "" ["   ✓ Found "; string_of_int (List.length files); " files in "; string_of_int scan_elapsed; "ms\n"]);
  
  (* 3. Create File entities *)
  println "3. Creating File entities...";
  let create_start = Time.Instant.now () in
  let file_entities = create_file_entities files in
  let create_elapsed = Time.Instant.elapsed create_start |> Time.Duration.to_millis in
  println (String.concat "" ["   ✓ Created "; string_of_int (List.length file_entities); " File entities in "; string_of_int create_elapsed; "ms\n"]);
  
  (* 4. State files in batches *)
  println "4. Stating files to database...";
  let batch_size = 300 in
  let state_elapsed = state_files_in_batches graph file_entities batch_size in
  println "   ✓ All files stated\n";
  
  (* Flush to ensure everything is written *)
  Poneglyph.flush graph;
  
  (* 5. Run multi-hop CodeDB queries *)
  println "5. Running multi-hop CodeDB queries...\n";
  
  (* Helper: Execute a Datalog query and return results *)
  let run_query query_str =
    match Poneglyph.execute_query graph ~query:query_str with
    | Error e ->
        println (String.concat "" ["Query error: "; e]);
        []
    | Ok results -> Iter.MutIterator.to_list results
  in
  
  (* Helper: Extract string value from substitution *)
  let get_string_value subst var =
    match Datalog.Substitution.lookup subst ~var with
    | Some (Datalog.Value.String s) -> Some s
    | Some (Datalog.Value.Uri s) -> Some s
    | _ -> None
  in
  
  (* Query 1: Find file that provides Std__Random (module → file) *)
  println "   Query 1: Which file provides module 'Std__Random'?";
  let (q1_result, q1_time) = timed_query "  " (fun () ->
    run_query "'ocaml:canonical_name'(M, \"Std__Random\"), 'codedb:provided_by'(M, F), 'codedb:path'(F, Path)"
  ) in
  (match q1_result with
  | subst :: _ ->
      (match get_string_value subst "Path" with
      | Some path -> println (String.concat "" ["     → "; path])
      | None -> println "     → Path not found in result")
  | [] -> println "     → No results found");
  
  (* Query 2: Find file that provides Tusk_planner__Solver (module → file) *)
  println "\n   Query 2: Which file provides module 'Tusk_planner__Solver'?";
  let (q2_result, q2_time) = timed_query "  " (fun () ->
    run_query "'ocaml:canonical_name'(M, \"Tusk_planner__Solver\"), 'codedb:provided_by'(M, F), 'codedb:path'(F, Path)"
  ) in
  (match q2_result with
  | subst :: _ ->
      (match get_string_value subst "Path" with
      | Some path -> println (String.concat "" ["     → "; path])
      | None -> println "     → Path not found in result")
  | [] -> println "     → No results found");
  
  (* Query 3: Find package for Poneglyph__Graph_store module (module → package) *)
  println "\n   Query 3: Which package contains 'Poneglyph__Graph_store'?";
  let (q3_result, q3_time) = timed_query "  " (fun () ->
    run_query "'ocaml:canonical_name'(M, \"Poneglyph__Graph_store\"), 'codedb:package'(M, Pkg)"
  ) in
  (match q3_result with
  | subst :: _ ->
      (match get_string_value subst "Pkg" with
      | Some pkg -> println (String.concat "" ["     → "; pkg])
      | None -> println "     → Package not found in result")
  | [] -> println "     → No results found");
  
  (* Summary of query performance *)
  println "\n   Query Performance:";
  println (String.concat "" ["     - Module→File (Std__Random): "; string_of_int q1_time; "ms"]);
  println (String.concat "" ["     - Module→File (Tusk_planner__Solver): "; string_of_int q2_time; "ms"]);
  println (String.concat "" ["     - Module→Package: "; string_of_int q3_time; "ms"]);
  
  (* Summary *)
  println "\n=== Summary ===";
  println "Indexing:";
  println (String.concat "" ["  - Files scanned: "; string_of_int (List.length files)]);
  println (String.concat "" ["  - Modules indexed: "; string_of_int (List.length (List.filter Option.is_some (List.map (fun f -> file_to_module_symbol f) file_entities)))]);
  println "\nPerformance:";
  println (String.concat "" ["  - Indexing: "; string_of_int (scan_elapsed + create_elapsed + Time.Duration.to_millis state_elapsed); "ms"]);
  println (String.concat "" ["  - Throughput: "; Float.to_string (Float.of_int (List.length files) /. (Float.of_int (Time.Duration.to_millis state_elapsed) /. 1000.0)); " files/sec"]);
  
  Poneglyph.close graph;
  Ok ()

let () =
  match main () with
  | Ok () -> ()
  | Error (Failure msg) ->
      Log.error msg;
      exit 1
  | Error exn ->
      Log.error (Exception.to_string exn);
      exit 1
