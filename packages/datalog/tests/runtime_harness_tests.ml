(* Runtime test harness - loads fixtures and runs them *)
open Std
open Datalog

module Eval = Evaluator.Make(Universe.InMemory)

(* Parse JSON value to Datalog Value *)
let json_to_value json =
  match json with
  | Data.Json.Int i -> Some (Value.Int i)
  | Data.Json.Float f ->
      let i = Float.to_int f in
      Some (Value.Int i)
  | Data.Json.String s -> Some (Value.String s)
  | _ -> None

(* Parse expected results from JSON (single query format) *)
let parse_expected json =
  match json with
  | Data.Json.Object fields ->
      let query_json = List.assoc_opt "query" fields in
      let result_json = List.assoc_opt "result" fields in
      
      (match query_json, result_json with
      | Some (Data.Json.String query), Some (Data.Json.Array results) ->
          Some (query, results)
      | _ -> None)
  | _ -> None

(* Parse multi-query format *)
let parse_multi_query_expected json =
  match json with
  | Data.Json.Object fields ->
      let queries_json = List.assoc_opt "queries" fields in
      (match queries_json with
      | Some (Data.Json.Array queries) ->
          let parsed = List.filter_map (fun query_obj ->
            match query_obj with
            | Data.Json.Object query_fields ->
                let query_str = List.assoc_opt "query" query_fields in
                let result_arr = List.assoc_opt "result" query_fields in
                (match query_str, result_arr with
                | Some (Data.Json.String q), Some (Data.Json.Array r) ->
                    Some (q, r)
                | _ -> None)
            | _ -> None
          ) queries in
          Some parsed
      | _ -> None)
  | _ -> None

(* Convert JSON result to substitution for comparison *)
let json_result_to_bindings json =
  match json with
  | Data.Json.Object fields ->
      let bindings = ref [] in
      List.iter (fun (var, value_json) ->
        match json_to_value value_json with
        | Some v -> bindings := (var, v) :: !bindings
        | None -> ()
      ) fields;
      Some !bindings
  | _ -> None

(* Check if two binding lists match (order doesn't matter) *)
let bindings_equal expected actual =
  List.for_all (fun (var, expected_val) ->
    match List.assoc_opt var actual with
    | Some actual_val -> Value.equal expected_val actual_val
    | None -> false
  ) expected
  && List.length expected = List.length actual



(* Discover fixtures in a specific subdirectory *)
let discover_fixtures_in_dir dir_name =
  let fixtures_dir = Path.v ("packages/datalog/tests/runtime/fixtures/" ^ dir_name) in
  let entries_iter =
    Fs.read_dir fixtures_dir
    |> Result.expect ~msg:("Failed to read fixtures directory: " ^ dir_name)
  in
  let entries = Iter.MutIterator.to_list entries_iter in

  List.filter_map
    (fun entry ->
      let path = Path.to_string (Path.join fixtures_dir entry) in
      if String.ends_with ~suffix:".datalog" path then
        let expected_path = path ^ ".expected" in
        let exists =
          Fs.exists (Path.v expected_path) |> Result.unwrap_or ~default:false
        in
        if exists then Some (path, expected_path) else None
      else None)
    entries
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* Discover single-query fixtures *)
let discover_single_query_fixtures () =
  discover_fixtures_in_dir "single-query"

(* Discover multi-query fixtures *)
let discover_multi_query_fixtures () =
  discover_fixtures_in_dir "multi-query"

(* Run a single fixture as a Test *)
let test_fixture_file fixture_path expected_path =
  (* Extract just the filename for display *)
  let name = Path.basename (Path.v fixture_path) in
  
  (* Load datalog file *)
  let datalog_content =
    Fs.read_to_string (Path.v fixture_path)
    |> Result.expect ~msg:("Failed to read datalog file: " ^ fixture_path)
  in
  
  (* Load expected file *)
  let expected_content =
    Fs.read_to_string (Path.v expected_path)
    |> Result.expect ~msg:("Failed to read expected file: " ^ expected_path)
  in
  
  (* Parse expected JSON *)
  let json =
    Data.Json.of_string expected_content
    |> Result.expect ~msg:("Failed to parse JSON in " ^ expected_path)
  in
  
  let (query_str, expected_results) = 
    match parse_expected json with
    | Some res -> res
    | None -> panic ("Invalid expected format in " ^ expected_path)
  in
  
  (* Parse the datalog program *)
  let cst = 
    match Parser.parse datalog_content with
    | Error diagnostics ->
        let diag_str = 
          List.map (fun d -> Parser.Diagnostic.to_string d) diagnostics 
          |> String.concat "; " 
        in
        panic (name ^ " - parse error: " ^ diag_str)
    | Ok cst -> cst
  in
  
  (* Convert CST to AST *)
  let program =
    Ast_from_cst.program_of_cst cst
    |> Result.expect ~msg:(name ^ " - AST conversion failed")
  in
  
  (* Convert facts from AST to (predicate, tuples) format *)
  let facts_grouped = ref [] in
  List.iter (fun fact ->
    let tuple = List.map (function
      | Term.Const v -> v
      | _ -> panic "Facts should not have variables"
    ) fact.Ast.args in
    
    match List.assoc_opt fact.predicate !facts_grouped with
    | Some existing ->
        facts_grouped := (fact.predicate, tuple :: existing) ::
          (List.remove_assoc fact.predicate !facts_grouped)
    | None ->
        facts_grouped := (fact.predicate, [tuple]) :: !facts_grouped
  ) program.facts;
  
  (* Build universe from facts *)
  let universe = Universe.InMemory.of_facts !facts_grouped in
  
  (* Add all rules *)
  let universe = List.fold_left (fun u rule ->
    Universe.InMemory.add_rule u rule
  ) universe program.rules in
  
  (* Evaluate to fixed point *)
  let universe = Eval.eval universe in
  
  (* Parse and execute query *)
  let query_cst =
    Parser.parse_query query_str
    |> Result.expect ~msg:(name ^ " - query parse failed")
  in
  
  let query =
    Ast_from_cst.query_of_cst query_cst
    |> Result.expect ~msg:(name ^ " - query AST conversion failed")
  in
  
  (* Execute query - handle both Single and Multi *)
  let results = match query with
    | Ast.Single atom -> Eval.query universe atom
    | Ast.Multi clauses -> Eval.multi_query universe clauses
  in
  
  (* Parse expected results *)
  let expected_bindings = 
    List.filter_map json_result_to_bindings expected_results 
  in
  
  (* Check count matches *)
  let expected_count = List.length expected_bindings in
  let actual_count = List.length results in
  if expected_count != actual_count then
    panic (name ^ ": Expected " ^ string_of_int expected_count ^ 
           " results, got " ^ string_of_int actual_count);
  
  (* Check each result matches *)
  List.iter (fun result_sub ->
    (* Convert Substitution to binding list *)
    let actual_bindings = Substitution.bindings result_sub in
    (* Check if this matches any expected binding *)
    let matches = List.exists (fun expected ->
      bindings_equal expected actual_bindings
    ) expected_bindings in
    if not matches then begin
      let bindings_str = 
        List.map (fun (var, value) -> 
          var ^ "=" ^ Value.to_string value
        ) actual_bindings
        |> String.concat ", "
      in
      panic ("Result doesn't match any expected binding: {" ^ bindings_str ^ "}")
    end
  ) results;
  
  Ok ()

(* Run a multi-query fixture test *)
let test_multi_query_fixture fixture_path expected_path =
  let name = Path.basename (Path.v fixture_path) in
  
  (* Load datalog file *)
  let datalog_content =
    Fs.read_to_string (Path.v fixture_path)
    |> Result.expect ~msg:("Failed to read datalog file: " ^ fixture_path)
  in
  
  (* Load expected file *)
  let expected_content =
    Fs.read_to_string (Path.v expected_path)
    |> Result.expect ~msg:("Failed to read expected file: " ^ expected_path)
  in
  
  (* Parse expected JSON *)
  let json =
    Data.Json.of_string expected_content
    |> Result.expect ~msg:("Failed to parse JSON in " ^ expected_path)
  in
  
  let queries = 
    match parse_multi_query_expected json with
    | Some qs -> qs
    | None -> panic ("Invalid multi-query format in " ^ expected_path)
  in
  
  (* Parse the datalog program *)
  let cst = 
    match Parser.parse datalog_content with
    | Error diagnostics ->
        let diag_str = 
          List.map (fun d -> Parser.Diagnostic.to_string d) diagnostics 
          |> String.concat "; " 
        in
        panic (name ^ " - parse error: " ^ diag_str)
    | Ok cst -> cst
  in
  
  (* Convert CST to AST *)
  let program =
    Ast_from_cst.program_of_cst cst
    |> Result.expect ~msg:(name ^ " - AST conversion failed")
  in
  
  (* Convert facts from AST to (predicate, tuples) format *)
  let facts_grouped = ref [] in
  List.iter (fun fact ->
    let tuple = List.map (function
      | Term.Const v -> v
      | _ -> panic "Facts should not have variables"
    ) fact.Ast.args in
    
    match List.assoc_opt fact.predicate !facts_grouped with
    | Some existing ->
        facts_grouped := (fact.predicate, tuple :: existing) ::
          (List.remove_assoc fact.predicate !facts_grouped)
    | None ->
        facts_grouped := (fact.predicate, [tuple]) :: !facts_grouped
  ) program.facts;
  
  (* Build universe from facts *)
  let universe = Universe.InMemory.of_facts !facts_grouped in
  
  (* Add all rules *)
  let universe = List.fold_left (fun u rule ->
    Universe.InMemory.add_rule u rule
  ) universe program.rules in
  
  (* Evaluate to fixed point *)
  let universe = Eval.eval universe in
  
  (* Test each query *)
  List.iter (fun (query_str, expected_results) ->
    (* Parse and execute query *)
    let query_cst =
      Parser.parse_query query_str
      |> Result.expect ~msg:(name ^ " - query parse failed: " ^ query_str)
    in
    
    let query =
      Ast_from_cst.query_of_cst query_cst
      |> Result.expect ~msg:(name ^ " - query AST conversion failed")
    in
    
    (* Execute query *)
    let results = match query with
      | Ast.Single atom -> Eval.query universe atom
      | Ast.Multi clauses -> Eval.multi_query universe clauses
    in
    
    (* Parse expected results *)
    let expected_bindings = 
      List.filter_map json_result_to_bindings expected_results 
    in
    
    (* Check count matches *)
    let expected_count = List.length expected_bindings in
    let actual_count = List.length results in
    if expected_count != actual_count then
      panic (name ^ " query '" ^ query_str ^ "': Expected " ^ string_of_int expected_count ^ 
             " results, got " ^ string_of_int actual_count);
    
    (* Check each result matches *)
    List.iter (fun result_sub ->
      let actual_bindings = Substitution.bindings result_sub in
      let matches = List.exists (fun expected ->
        bindings_equal expected actual_bindings
      ) expected_bindings in
      if not matches then begin
        let bindings_str = 
          List.map (fun (var, value) -> 
            var ^ "=" ^ Value.to_string value
          ) actual_bindings
          |> String.concat ", "
        in
        panic ("Query '" ^ query_str ^ "' result doesn't match: {" ^ bindings_str ^ "}")
      end
    ) results
  ) queries;
  
  Ok ()

let () =
  Miniriot.run
    ~main:(fun ~args ->
      (* Discover both types of fixtures *)
      let single_query_fixtures = discover_single_query_fixtures () in
      let multi_query_fixtures = discover_multi_query_fixtures () in
      
      (* Create tests for single-query fixtures *)
      let single_query_tests =
        List.map
          (fun (fixture_path, expected_path) ->
            let name = Path.basename (Path.v fixture_path) in
            Test.case name (fun () -> test_fixture_file fixture_path expected_path))
          single_query_fixtures
      in
      
      (* Create tests for multi-query fixtures *)
      let multi_query_tests =
        List.map
          (fun (fixture_path, expected_path) ->
            let name = "[multi] " ^ Path.basename (Path.v fixture_path) in
            Test.case name (fun () -> test_multi_query_fixture fixture_path expected_path))
          multi_query_fixtures
      in
      
      (* Combine all tests *)
      let tests = single_query_tests @ multi_query_tests in
      
      Test.Cli.main ~name:"datalog-fixtures" ~tests ~args)
    ~args:Env.args ()
