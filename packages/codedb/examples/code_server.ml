open Std

(** Standalone Codedb Server
    
    This demonstrates:
    1. Loading workspace and toolchain
    2. Starting Codedb supervisor with multiple services
    3. Triggering workspace indexing
    4. File watching (future)
    
    The supervisor manages:
    - Internal server (indexing engine)
    - File watcher (monitors changes)
*)

let main () =
  (* Set up logging - only show warnings and errors from subsystems *)
  Log.set_level Log.Warn;
  
  println "\n🔍 Codedb - Code Intelligence Engine\n";

  (* 1. Get workspace root *)
  let workspace_root =
    Env.current_dir ()
    |> Result.expect ~msg:"Failed to get current directory"
  in

  (* 2. Load workspace first (needed for toolchain config) *)
  println "📦 Loading workspace...";
  let workspace =
    Tusk_model.Workspace_manager.scan workspace_root
    |> Result.map fst
    |> Result.expect ~msg:"Failed to scan workspace"
  in
  println ("   ✓ Found " ^ Int.to_string (List.length workspace.packages) ^ " packages");

  (* 3. Detect toolchain *)
  println "🔧 Detecting OCaml toolchain...";
  let toolchain_config =
    Tusk_model.Toolchain_config.from_workspace workspace
  in

  let toolchain =
    Tusk_toolchain.init ~config:toolchain_config
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in
  
  println "   ✓ Toolchain ready";

  (* 4. Configure Codedb *)
  let db_path = Path.(workspace_root / v ".codedb.pone") in
  let config =
    Codedb.Config.create
      ~workspace_root
      ~toolchain
      ~workspace
      ~db_path
      ()
  in

  (* 5. Start Codedb supervisor *)
  println "🚀 Starting Codedb supervisor...";
  let _supervisor = Codedb.Service.start config in
  println "   ✓ Supervisor running";

  println "\n✨ Indexing workspace (this may take a moment)...\n";
  
  (* Give indexing time to complete *)
  sleep (Time.Duration.from_secs 3);
  
  println "\n✅ Codedb server running!";
  println "📂 Database: .codedb/";
  println "👀 File watcher: active (monitoring packages/)";
  
  (* Demo: Run some example queries *)
  println "\n🔍 Running example queries...\n";
  
  let db =
    Poneglyph.open_exclusive ~data_dir:(Path.to_string db_path) ()
    |> Result.expect ~msg:"Failed to open database for queries"
  in
  
  (* Query 1: Find a specific module *)
  println "Query 1: Find Tusk_cli.Cli module";
  let cli_uri = Codedb.Schema.OCaml.Module.uri "Tusk_cli.Cli" in
  let cli_name = Poneglyph.get db ~entity:cli_uri ~attr:Codedb.Schema.OCaml.canonical_name in
  (match cli_name with
  | Some (Poneglyph.Fact.String name) -> 
      println ("  ✓ Found: " ^ name);
      let cli_path = Poneglyph.get db ~entity:cli_uri ~attr:Codedb.Schema.Codedb.path in
      (match cli_path with
      | Some (Poneglyph.Fact.String path) -> println ("    Path: " ^ path)
      | _ -> ());
      
      (* Show the file it's provided by *)
      let provided_by = Poneglyph.get db ~entity:cli_uri ~attr:Codedb.Schema.Codedb.provided_by in
      (match provided_by with
      | Some (Poneglyph.Fact.Uri file_uri) -> 
          println ("    Provided by: " ^ Poneglyph.Uri.to_string file_uri)
      | _ -> ())
  | _ -> println "  ✗ Not found");
  
  (* Query 2: Find all modules in std package *)
  println "\nQuery 2: Count modules in 'std' package";
  let std_package_uri = Codedb.Schema.Tusk.Package.uri "std" in
  let std_modules = 
    Poneglyph.find_entities db 
      ~attr:Codedb.Schema.Codedb.package 
      ~value:(Poneglyph.Fact.Uri std_package_uri)
  in
  let std_count = ref 0 in
  let rec count_modules () =
    match Iter.MutIterator.next std_modules with
    | Some _ -> std_count := !std_count + 1; count_modules ()
    | None -> !std_count
  in
  let count = count_modules () in
  println ("  ✓ Found " ^ Int.to_string count ^ " modules in std");
  
  (* Query 3: Find recent ML files *)
  println "\nQuery 3: Find .ml files in codedb package";
  let codedb_package_uri = Codedb.Schema.Tusk.Package.uri "codedb" in
  let codedb_modules = 
    Poneglyph.find_entities db 
      ~attr:Codedb.Schema.Codedb.package 
      ~value:(Poneglyph.Fact.Uri codedb_package_uri)
  in
  let ml_count = ref 0 in
  let rec count_ml_files () =
    match Iter.MutIterator.next codedb_modules with
    | Some uri ->
        (match Poneglyph.get db ~entity:uri ~attr:Codedb.Schema.Codedb.kind with
        | Some (Poneglyph.Fact.String "ml") -> ml_count := !ml_count + 1
        | _ -> ());
        count_ml_files ()
    | None -> !ml_count
  in
  let ml_files = count_ml_files () in
  println ("  ✓ Found " ^ Int.to_string ml_files ^ " .ml files in codedb package");
  
  (* Query 4: Transitive query demo - find file analysis chain *)
  println "\nQuery 4: Transitive query demo (analysis_of relationships)";
  println "  Note: Full dependency graph requires import/depends_on relationships";
  println "  (Future enhancement: index module imports for true dependency graph)";
  
  (* Find first analysis entity *)
  let first_analysis = 
    Poneglyph.find_entities db
      ~attr:Codedb.Schema.Codedb.analysis_of
      ~value:(Poneglyph.Fact.Uri (Poneglyph.Uri.of_string "codedb:"))  (* Any URI *)
  in
  (match Iter.MutIterator.next first_analysis with
  | Some analysis_uri ->
      println ("  ✓ Example analysis entity: " ^ Poneglyph.Uri.to_string analysis_uri);
      
      (* Get what file it analyzed *)
      (match Poneglyph.get db ~entity:analysis_uri ~attr:Codedb.Schema.Codedb.analysis_of with
      | Some (Poneglyph.Fact.Uri file_uri) ->
          println ("    Analyzed file: " ^ Poneglyph.Uri.to_string file_uri)
      | _ -> ());
      
      (* Get when it was analyzed *)
      (match Poneglyph.get db ~entity:analysis_uri ~attr:Codedb.Schema.Codedb.analyzed_at with
      | Some (Poneglyph.Fact.DateTime dt) ->
          println ("    Analyzed at: " ^ Datetime.to_iso8601 dt)
      | _ -> ())
  | None -> println "  ✗ No analysis entities found");
  
  println "\n✅ Queries complete! Server will continue running.\n";
  println "Press Ctrl+C to stop.\n";
  
  Poneglyph.close db;
  
  (* Keep process alive *)
  let rec wait () =
    sleep (Time.Duration.from_secs 3600);
    wait ()
  in
  wait ()

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      try
        main ();
        Ok ()
      with
      | exn ->
          Log.error ("Server failed: " ^ Exception.to_string exn);
          Error exn)
    ~args:[] ()
