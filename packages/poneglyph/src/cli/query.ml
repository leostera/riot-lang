open Std
open Std.Iter

let command =
  let open ArgParser in
  let open Arg in
  command "query"
  |> about "Run a Datalog query"
  |> args [
      positional "db" |> help "Database path";
      positional "query" |> help "Datalog query string";
    ]

let execute_query db ~query =
  (* Phase 0: Query-only Datalog - no rules *)
  let query_str = query in
  
  (* 1. Create Datalog storage from Graph_store *)
  let storage = db in
  
  (* 2. Create universe with Poneglyph storage *)
  let module U = Datalog.Universe.Make(Datalog_storage.PoneglyphStorage) in
  let universe = U.create storage in
  
  (* 3. Parse query *)
  (match Datalog.Parser.parse_query query_str with
  | Error diagnostics ->
      let diag_str = 
        List.map (fun d -> Datalog.Parser.Diagnostic.to_string d) diagnostics 
        |> String.concat "; " 
      in
      Error ("Query parse error: " ^ diag_str)
  | Ok query_cst ->
      (match Datalog.Ast_from_cst.query_of_cst query_cst with
      | Error e -> Error ("Failed to convert query to AST: " ^ e)
      | Ok query_ast ->
           (* Execute query based on type *)
           (match query_ast with
            | Datalog.Ast.Single atom ->
                (* Single-goal query - pure streaming! *)
                let module Eval = Datalog.Evaluator.Make(U) in
                let results = Eval.query universe atom in
                Ok results
           | Datalog.Ast.Multi clauses ->
                (* Multi-goal query - streaming join! *)
                let module Eval = Datalog.Evaluator.Make(U) in
                let results = Eval.multi_query universe clauses in
                Ok results)))

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
  let query_str = get_one matches "query" |> Option.expect ~msg:"query required" in
  
  (* Measure database opening time *)
  let db, db_open_time = Timer.measure (fun () ->
    match Graph_store.open_shared ~data_dir:db_path with
    | Error e ->
        println ("Error: Failed to open database: " ^ e);
        println "Note: Database may be locked by another process (e.g., a write operation)";
        panic e
    | Ok db -> db
  ) in
  
  (* Measure query execution time *)
  let results, query_time = Timer.measure (fun () ->
    match execute_query db ~query:query_str with
    | Error e ->
        Graph_store.close db;
        println ("Query error: " ^ e);
        panic ("Query failed: " ^ e)
    | Ok results -> results
  ) in
  
  (* Stream results as JSON - print each result as it's produced *)
  let rec print_results iter =
    match Iter.MutIterator.next iter with
    | None -> ()
    | Some subst ->
        (* Convert substitution to JSON and print *)
        let json = Datalog.Substitution.to_json subst in
        println (Data.Json.to_string json);
        
        (* Continue with next result *)
        print_results iter
  in
  print_results results;
  
  Graph_store.close db;
  
  (* Print performance summary to stderr *)
  let total = Time.Duration.add db_open_time query_time in
  eprint "\n";
  eprint "Performance:\n";
  eprint ("  DB open: " ^ (Time.Duration.to_secs_string ~precision:3 db_open_time) ^ "s\n");
  eprint ("  Query:   " ^ (Time.Duration.to_secs_string ~precision:3 query_time) ^ "s\n");
  eprint ("  Total:   " ^ (Time.Duration.to_secs_string ~precision:3 total) ^ "s\n");
  
  Ok ()
