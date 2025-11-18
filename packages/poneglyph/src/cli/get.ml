open Std

let command =
  let open ArgParser in
  let open Arg in
  command "get"
  |> about "Get all facts for an entity"
  |> args [
      positional "db" |> help "Database path";
      positional "entity" |> help "Entity URI (e.g., person:alice)";
    ]

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
  let entity_str = get_one matches "entity" |> Option.expect ~msg:"entity required" in
  
  let entity = Model.Uri.of_string entity_str in
  
  (* Open database for reading only (shared lock) *)
  match Graph_store.open_shared ~data_dir:db_path with
  | Error e ->
      println ("Error: Failed to open database: " ^ e);
      println "Note: Database may be locked by another process";
      Error (Failure e)
  | Ok db ->
      (* Get facts for entity *)
      let facts = Graph_store.get_current_facts db ~entity in
      
      (* Build JSON object using Std.Data.Json *)
      let open Data.Json in
      let fields = ref [] in
      
      Iter.MutIterator.for_each facts ~fn:(fun fact ->
        let attr = Model.Uri.to_string fact.Model.Fact.attribute in
        let value_json = match fact.Model.Fact.value with
          | Model.Fact.String s -> String s
          | Model.Fact.Int n -> Int n
          | Model.Fact.Bool b -> Bool b
          | Model.Fact.Float f -> Float f
          | Model.Fact.Uri u -> String (Model.Uri.to_string u)
          | Model.Fact.DateTime dt -> String (Datetime.to_iso8601 dt)
        in
        fields := (attr, value_json) :: !fields
      );
      
      let json = Object (List.rev !fields) in
      println (Data.Json.to_string json);
      
      Graph_store.close db;
      Ok ()
