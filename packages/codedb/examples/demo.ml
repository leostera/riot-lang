open Std

module Stats = struct
  let command =
    let open ArgParser in
    let open Arg in
    command "stats"
    |> about "Show CodeDB statistics"
    |> args [
        positional "db" |> help "Database path (.codedb.pone)";
      ]

  let run matches =
    let open ArgParser in
    let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
    
    (* Open database for reading *)
    match Poneglyph.open_shared ~data_dir:db_path with
    | Error e ->
        println ("Error: Failed to open database: " ^ e);
        Error (Failure e)
    | Ok graph ->
        let stats = Poneglyph.stats graph in
        
        println ("CodeDB Statistics:");
        println ("  Total facts: " ^ string_of_int (List.assoc "current_facts" stats));
        println ("  Database: " ^ db_path);
        
        Poneglyph.close graph;
        Ok ()
end

let cli =
  let open ArgParser in
  command "codedb"
  |> version "0.1.0"
  |> about "Code database with Datalog queries"
  |> subcommands [
      Stats.command;
    ]

let main ~args =
  match ArgParser.get_matches cli args with
  | Error err ->
      ArgParser.print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> 
      match ArgParser.get_subcommand matches with
      | Some ("stats", sub_matches) -> Stats.run sub_matches
      | Some (cmd, _) -> 
          println ("Unknown command: " ^ cmd);
          Error (Failure ("Unknown command: " ^ cmd))
      | None ->
          ArgParser.print_help cli;
          Ok ()

let () =
  match main ~args:Env.args with
  | Ok () -> ()
  | Error (Failure msg) ->
      Log.error msg;
      exit 1
  | Error exn ->
      Log.error (Exception.to_string exn);
      exit 1
