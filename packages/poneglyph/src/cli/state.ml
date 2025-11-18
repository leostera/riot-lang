open Std
open Std.UUID

let parse_value_from_matches matches =
  (* Parse value based on which type flag was provided *)
  let open ArgParser in
  let open Model.Fact in
  
  (* Check each value type option *)
  match get_one matches "string" with
  | Some s -> Ok (String s)
  | None -> (
      match get_one matches "int" with
      | Some i_str -> (
          match int_of_string_opt i_str with
          | Some i -> Ok (Int i)
          | None -> Error ("Invalid integer: " ^ i_str)
        )
      | None -> (
          match get_one matches "bool" with
          | Some b_str -> (
              match b_str with
              | "true" -> Ok (Bool true)
              | "false" -> Ok (Bool false)
              | _ -> Error ("Invalid boolean (use 'true' or 'false'): " ^ b_str)
            )
          | None -> (
              match get_one matches "float" with
              | Some f_str -> (
                  match float_of_string_opt f_str with
                  | Some f -> Ok (Float f)
                  | None -> Error ("Invalid float: " ^ f_str)
                )
              | None -> (
                  match get_one matches "uri" with
                  | Some u_str -> Ok (Uri (Model.Uri.of_string u_str))
                  | None -> (
                      match get_one matches "datetime" with
                      | Some dt_str -> (
                          match Datetime.parse dt_str with
                          | Ok dt -> Ok (DateTime dt)
                          | Error _ -> Error ("Invalid datetime format: " ^ dt_str)
                        )
                      | None -> Error "No value type specified (use --string, --int, --bool, --float, --uri, or --datetime)"
                    )
                )
            )
        )
    )

let command =
  let open ArgParser in
  let open Arg in
  command "state"
  |> about "Add a single fact to the database"
  |> args [
      positional "db" |> help "Database path";
      positional "entity" |> help "Entity URI (e.g., person:alice)";
      positional "attribute" |> help "Attribute URI (e.g., name)";
      
      (* Value type options (mutually exclusive in practice) *)
      option "string" |> long "string" |> help "String value";
      option "int" |> long "int" |> help "Integer value";
      option "bool" |> long "bool" |> help "Boolean value (true/false)";
      option "float" |> long "float" |> help "Float value";
      option "uri" |> long "uri" |> help "URI reference value";
      option "datetime" |> long "datetime" |> help "ISO 8601 datetime";
    ]

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
  let entity_str = get_one matches "entity" |> Option.expect ~msg:"entity required" in
  let attr_str = get_one matches "attribute" |> Option.expect ~msg:"attribute required" in
  
  (* Parse value from type options *)
  (match parse_value_from_matches matches with
   | Error e ->
       println ("Error: " ^ e);
       Error (Failure e)
   | Ok value ->
       (* Create fact *)
       let entity = Model.Uri.of_string entity_str in
       let attr = Model.Uri.of_string attr_str in
       let source = Model.Uri.of_string "cli:state" in
       
       let fact = Model.Fact.make
         ~source
         ~entity
         ~attribute:attr
         ~value
         ~stated_at:(Datetime.now ())
         ~tx_id:(UUID.v7_monotonic ()) in
       
        (* Open database for writing *)
        match Graph_store.open_exclusive ~data_dir:db_path () with
        | Error e ->
            println ("Error: Failed to open database: " ^ e);
            println "Note: Database may be locked by another process (e.g., a concurrent query or write)";
            Error (Failure e)
        | Ok db ->
            (* State fact *)
            let _count = Graph_store.state db [fact] in
            
            (* CLI-SPECIFIC: Aggressively compact ALL tiers before close.
               This is critical because open/write/close pattern creates many tiny SSTables.
               We compact tier-by-tier (0 through 5) to merge them before closing.
               This keeps the database compact even with hundreds of individual writes. *)
            let rec compact_all_tiers tier =
              if tier > 5 then ()  (* Stop at tier 5 *)
              else
                match Graph_store.compact_tier db ~tier ~threshold:2 ~max_merge:50 () with
                | Ok true -> 
                    (* More work on this tier - keep compacting *)
                    compact_all_tiers tier
                | Ok false -> 
                    (* This tier is done - move to next tier *)
                    compact_all_tiers (tier + 1)
                | Error _ -> 
                    (* Ignore errors and try next tier *)
                    compact_all_tiers (tier + 1)
            in
            compact_all_tiers 0;
            
            (* Close database *)
            Graph_store.close db;
            
            println ("Stated 1 fact to " ^ db_path);
            Ok ()
   )
