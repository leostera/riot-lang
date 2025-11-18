open Std

let command =
  let open ArgParser in
  let open Arg in
  command "stats"
  |> about "Show database statistics"
  |> args [
      positional "db" |> help "Database path";
    ]

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
  
  (* Open database for reading only (shared lock) *)
  match Graph_store.open_shared ~data_dir:db_path with
  | Error e ->
      let json = Data.Json.obj [
        ("error", Data.Json.string e);
      ] in
      println (Data.Json.to_string json);
      Error (Failure e)
  | Ok db ->
      (* Get detailed statistics from Graph_store *)
      let detailed_stats = Graph_store.get_detailed_stats db in
      
      (* Get basic entity count (works for all backends) *)
      let entities = Graph_store.count_entities db in
      
      (* For LSM, extract counts from detailed_stats to avoid warnings *)
      (* For other backends, use the count functions *)
      let facts, current_facts = 
        match Data.Json.get_object detailed_stats with
        | Some fields ->
            (* Check if this is LSM backend (has "indices" field) *)
            (match List.find_opt (fun (k, _) -> k = "indices") fields with
            | Some (_, indices_json) ->
                (* LSM backend - extract total_entries from fact index *)
                (match Data.Json.get_object indices_json with
                | Some indices ->
                    let fact_entries = 
                      match List.find_opt (fun (k, _) -> k = "fact") indices with
                      | Some (_, fact_json) ->
                          (match Data.Json.get_object fact_json with
                          | Some fact_fields ->
                              (match List.find_opt (fun (k, _) -> k = "total_entries") fact_fields with
                              | Some (_, Data.Json.Int n) -> n
                              | _ -> 0)
                          | None -> 0)
                      | None -> 0
                    in
                    (fact_entries, fact_entries) (* Use same for both for now *)
                | None -> (0, 0))
            | None ->
                (* Not LSM - use count functions *)
                (Graph_store.count_facts db, Graph_store.count_current_facts db))
        | None ->
            (* Fallback to count functions *)
            (Graph_store.count_facts db, Graph_store.count_current_facts db)
      in
      
      (* Merge basic counts with detailed stats *)
      let json = 
        let base_fields = [
          ("entities", Data.Json.int entities);
          ("facts", Data.Json.int facts);
          ("current_facts", Data.Json.int current_facts);
        ] in
        
        (* Merge with detailed stats *)
        match Data.Json.get_object detailed_stats with
        | Some fields ->
            Data.Json.obj (base_fields @ fields)
        | None -> Data.Json.obj base_fields
      in
      
      println (Data.Json.to_string json);
      
      Graph_store.close db;
      Ok ()
