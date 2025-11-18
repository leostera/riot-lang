open Std
open Std.Data

let command =
  let open ArgParser in
  let open Arg in
  command "load"
  |> about "Bulk import facts from JSON file"
  |> args [
      positional "db" |> help "Database path";
      positional "file" |> help "JSON file path (use '-' for stdin)";
    ]

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db path required" in
  let json_file = get_one matches "file" |> Option.expect ~msg:"file path required" in
  
  (* Read JSON from file *)
  let json_str =
    if json_file = "-" then
      Error (Failure "stdin input not yet supported")
    else
      (match Fs.read (Path.v json_file) with
       | Error _ ->
           println ("Error: Failed to read file: " ^ json_file);
           Error (Failure ("Failed to read file: " ^ json_file))
       | Ok contents -> Ok contents
      )
  in
  
  match json_str with
  | Error e -> Error e
  | Ok json_str ->
      (* For now, just report that JSON loading is not yet implemented *)
      (* TODO: Implement JSON fact loading once we decide on the format *)
      println "Error: JSON loading not yet implemented";
      println "Please use the 'state' command to add facts individually";
      Error (Failure "JSON loading not yet implemented")
