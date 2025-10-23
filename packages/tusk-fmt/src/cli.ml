open Std
open Std.Iter

type format_status =
  | Success of { file : Path.t; changed : bool }
  | Failed of { file : Path.t; error : string }

let rec walk_dir dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      let entry_list = MutIterator.to_list entries in
      List.concat_map
        (fun entry ->
          let entry_path = Path.(dir / entry) in
          match Fs.is_dir entry_path with
          | Ok true ->
              if
                String.starts_with ~prefix:"." (Path.to_string entry)
                || String.equal (Path.to_string entry) "_build"
                || String.equal (Path.to_string entry) "target"
              then []
              else walk_dir entry_path
          | Ok false | Error _ ->
              let path_str = Path.to_string entry_path in
              if
                String.ends_with ~suffix:".ml" path_str
                || String.ends_with ~suffix:".mli" path_str
              then [ entry_path ]
              else [])
        entry_list

let collect_ocaml_files root = walk_dir root

let format_file write quiet file_path =
  let result =
    match Fs.read file_path with
    | Error (Fs.SystemError err) -> Failed { file = file_path; error = err }
    | Ok source -> (
        let tokens = Syn.tokenize source in
        let parse_result = Syn.Parser.parse_implementation ~source tokens in
        match parse_result.diagnostics with
        | [] ->
            let formatted = Formatter.format parse_result.tree in
            let changed = not (String.equal source formatted) in
            if write && changed then
              match Fs.write formatted file_path with
              | Ok () -> Success { file = file_path; changed }
              | Error (Fs.SystemError err) ->
                  Failed { file = file_path; error = err }
            else Success { file = file_path; changed }
        | diagnostics ->
            let errors =
              List.map Syn.Diagnostic.to_string diagnostics
              |> String.concat "\n  "
            in
            Failed { file = file_path; error = errors })
  in
  (match result with
  | Success { file; changed } when not write -> (
      println "@%s:" (Path.to_string file);
      match Fs.read file_path with
      | Ok source ->
          let tokens = Syn.tokenize source in
          let parse_result = Syn.Parser.parse_implementation ~source tokens in
          let formatted = Formatter.format parse_result.tree in
          println "%s" formatted
      | Error _ -> ())
  | Failed { file; error } ->
      println "\027[1;31mError formatting %s:\027[0m" (Path.to_string file);
      println "%s" error
  | Success { file; changed = true } when write && not quiet ->
      println "\027[1;32m✓ %s\027[0m" (Path.to_string file)
  | Success { file; changed = false } when write && not quiet ->
      println "\027[1;90m- %s (unchanged)\027[0m" (Path.to_string file)
  | _ -> ());
  result

let cli =
  let open ArgParser in
  let open Arg in
  command "tusk_fmt" |> version "0.1.0"
  |> about "Zero-configuration OCaml formatter"
  |> args
       [
         flag "check" |> long "check"
         |> help "Check if files need formatting without modifying them";
         flag "write" |> short 'w' |> long "write"
         |> help
              "Write formatted output back to files (default: print to stdout)";
         flag "quiet" |> short 'q' |> long "quiet"
         |> help "Only show failures, suppress successful file messages";
         positional "path" |> required false
         |> help "Path to format (default: .)";
       ]

let main ~args:argv =
  let open ArgParser in
  match get_matches cli argv with
  | Error err ->
      print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> (
      let check_only = get_flag matches "check" in
      let write = get_flag matches "write" in
      let quiet = get_flag matches "quiet" in
      let root =
        match get_path matches "path" with Some p -> p | None -> Path.v "."
      in

      if check_only && write then (
        Log.error "--check and --write are mutually exclusive";
        Error (Failure "Invalid flags"))
      else
        match Fs.exists root with
        | Error (Fs.SystemError err) ->
            Log.error "Failed to access %s: %s" (Path.to_string root) err;
            Error (Failure "Path does not exist")
        | Ok false ->
            Log.error "Path does not exist: %s" (Path.to_string root);
            Error (Failure "Path does not exist")
        | Ok true ->
            println "🎨 Formatting OCaml files...";
            println "";
            let files =
              match Fs.is_dir root with
              | Ok true -> collect_ocaml_files root
              | Ok false | Error _ ->
                  let path_str = Path.to_string root in
                  if
                    String.ends_with ~suffix:".ml" path_str
                    || String.ends_with ~suffix:".mli" path_str
                    || String.ends_with ~suffix:".actual" path_str
                  then [ root ]
                  else []
            in
            let file_count = List.length files in
            if file_count = 0 then (
              println "No OCaml files found.";
              Ok ())
            else
              let concurrency = min System.available_parallelism 50 in
              let concurrency = max concurrency 1 in
              println "Found %d files, using %d workers" file_count concurrency;
              println "";

              let results =
                WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files
                  ~fn:(fun file -> format_file write quiet file)
                  ()
              in

              println "";

              let successes =
                List.filter_map
                  (fun (_idx, result) ->
                    match result with
                    | Success { file; changed } -> Some (file, changed)
                    | Failed _ -> None)
                  results
              in
              let failures =
                List.filter_map
                  (fun (_idx, result) ->
                    match result with
                    | Failed { file; error } -> Some (file, error)
                    | Success _ -> None)
                  results
              in

              let changed_count =
                List.filter (fun (_file, changed) -> changed) successes
                |> List.length
              in
              let unchanged_count = List.length successes - changed_count in
              let failed_count = List.length failures in

              if failed_count > 0 then (
                println "\027[1;31mFailed to format %d files:\027[0m"
                  failed_count;
                println "";
                List.iter
                  (fun (file, error) ->
                    println "  \027[1;31m✗\027[0m %s" (Path.to_string file);
                    println "    %s" error;
                    println "")
                  failures;
                println "");

              if check_only then
                if changed_count > 0 then (
                  println "\027[1;33m%d files need formatting:\027[0m"
                    changed_count;
                  List.iter
                    (fun (file, changed) ->
                      if changed then println "  • %s" (Path.to_string file))
                    successes;
                  println "";
                  println "Run 'tusk_fmt --write' to format these files.";
                  Error (Failure "Files need formatting"))
                else (
                  println "\027[1;32m✓ All files are formatted correctly\027[0m";
                  println "";
                  Ok ())
              else if write then (
                println "Summary:";
                println "  \027[1;32m✓\027[0m %d files formatted" changed_count;
                println "  \027[1;90m-\027[0m %d files unchanged"
                  unchanged_count;
                if failed_count > 0 then
                  println "  \027[1;31m✗\027[0m %d files failed" failed_count;
                println "";
                if failed_count > 0 then Error (Failure "Some files failed")
                else Ok ())
              else (
                println "Summary (read-only mode):";
                println "  \027[1;32m✓\027[0m %d files would be formatted"
                  changed_count;
                println "  \027[1;90m-\027[0m %d files unchanged"
                  unchanged_count;
                if failed_count > 0 then
                  println "  \027[1;31m✗\027[0m %d files failed" failed_count;
                println "";
                if changed_count > 0 then
                  println "Run 'tusk_fmt --write' to format these files.";
                println "";
                if failed_count > 0 then Error (Failure "Some files failed")
                else Ok ()))
