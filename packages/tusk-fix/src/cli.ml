open Std

let command =
  let open ArgParser in
  let open Arg in
  command "fix"
  |> about "Lint OCaml code and apply safe fixes"
  |> args
       [
         flag "check" |> long "check"
         |> help "Check for fixable issues without modifying files";
         option "explain" |> long "explain"
         |> help "Explain a diagnostic code (e.g. F0001)";
         option "format" |> long "format"
         |> possible_values [ "text"; "json" ]
         |> help "Output format (text or json)";
         positional "path"
         |> required false
         |> help
              "OCaml file or directory to scan (default: workspace packages or current directory)";
       ]

let default_path () =
  let cwd =
    Env.current_dir ()
    |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace_root =
    match Fix_config.load_scope ~cwd with
    | Some scope -> Fix_config.workspace_root scope
    | None -> cwd
  in
  let packages_dir = Path.(workspace_root / Path.v "packages") in
  if Fs.is_dir packages_dir |> Result.unwrap_or ~default:false then
    packages_dir
  else cwd

let resolve_target matches =
  match ArgParser.get_path matches "path" with
  | Some path -> path
  | None -> default_path ()

let resolve_files target =
  match Fs.is_dir target with
  | Ok true ->
      let scanner = File_scanner.create ~root:target () in
      File_scanner.scan scanner
  | Ok false | Error _ -> [ target ]

let relative_to_cwd path =
  let cwd =
    Env.current_dir ()
    |> Result.expect ~msg:"Failed to get current directory"
  in
  match Path.strip_prefix path ~prefix:cwd with
  | Ok rel_path -> Path.to_string rel_path
  | Error _ -> Path.to_string path

let print_diagnostics result =
  let grouped = Diagnostic.group_diagnostics result.Runner.diagnostics in
  List.iter
    (fun grouped_diag ->
      print
        (Diagnostic.grouped_to_formatted_output ~file:result.file
           ~source:result.final_source grouped_diag))
    grouped

let print_parse_diagnostics result =
  if List.length result.Runner.parse_diagnostics > 0 then
    Syn.DiagnosticReporter.print ~file:(Path.to_string result.file)
      ~source:result.final_source result.parse_diagnostics

let print_text_result mode result =
  let rel = relative_to_cwd result.Runner.file in
  match result.error with
  | Some error ->
      println ("\027[1;31m✗\027[0m " ^ rel);
      println ("  " ^ error)
  | None ->
      if result.changed then
        println
          ("\027[1;32m✓\027[0m " ^ rel ^ " (applied "
         ^ Int.to_string (List.length result.applied_fixes)
         ^ " safe fixes)");
      if List.length result.parse_diagnostics > 0 then (
        println
          ("\027[1;31m✗\027[0m " ^ rel ^ " ("
         ^ Int.to_string (List.length result.parse_diagnostics)
         ^ " parse issues)");
        print_parse_diagnostics result);
      if List.length result.diagnostics > 0 then (
        let prefix =
          match mode with
          | Runner.Check -> "\027[1;31m✗\027[0m "
          | Runner.Apply -> "\027[1;33m!\027[0m "
        in
        let suffix =
          match mode with
          | Runner.Check -> "issues found"
          | Runner.Apply -> "issues remain"
        in
        println
          (prefix ^ rel ^ " (" ^ Int.to_string (List.length result.diagnostics)
         ^ " " ^ suffix ^ ")");
        print_diagnostics result)

let print_text_summary mode summary =
  println "";
  match mode with
  | Runner.Check ->
      if summary.Runner.remaining_diagnostics = 0 && summary.failed_files = 0 then
        println
          ("\027[1;32m✓\027[0m No issues found in "
         ^ Int.to_string summary.total_files ^ " files")
      else
        println
          ("\027[1;31m✗\027[0m Found "
         ^ Int.to_string summary.remaining_diagnostics ^ " issues across "
         ^ Int.to_string summary.total_files ^ " files")
  | Runner.Apply ->
      if
        summary.remaining_diagnostics = 0
        && summary.failed_files = 0
      then
        println
          ("\027[1;32m✓\027[0m Applied "
         ^ Int.to_string summary.applied_fixes ^ " safe fixes across "
         ^ Int.to_string summary.changed_files ^ " files")
      else
        println
          ("\027[1;33m!\027[0m Applied "
         ^ Int.to_string summary.applied_fixes ^ " safe fixes across "
         ^ Int.to_string summary.changed_files ^ " files; "
         ^ Int.to_string summary.remaining_diagnostics ^ " issues remain")

let explain_code code =
  match Diagnostic_code.explain code with
  | Some entry ->
      print (Diagnostic_code.format_explanation entry);
      Ok ()
  | None -> Error (Failure ("Unknown tusk-fix diagnostic code: " ^ code))

let run_with_coordinator ~format ~mode ~scope files =
  let concurrency =
    let recommended = System.available_parallelism in
    if recommended <= 0 then 1 else recommended
  in
  if format = Reporter.Text then
    println
      ("Scanning " ^ Int.to_string (List.length files) ^ " files with "
     ^ Int.to_string concurrency ^ " workers...");
  let owner = self () in
  ignore (Coordinator.start { files; concurrency; mode; scope; owner });
  let rec loop results_rev =
    let selector = function
      | Messages.FileResult result -> `select (`FileResult result)
      | Messages.AllComplete summary -> `select (`AllComplete summary)
      | _ -> `skip
    in
    match receive ~selector () with
    | `FileResult { Messages.result; _ } ->
        (match format with
        | Reporter.Text -> print_text_result mode result
        | Reporter.Json -> ());
        loop (result :: results_rev)
    | `AllComplete summary ->
        let result = Runner.{ files = List.rev results_rev; summary } in
        (match format with
        | Reporter.Json ->
            print (Data.Json.to_string (Runner.run_result_to_json result));
            print "\n"
        | Reporter.Text -> print_text_summary mode summary);
        if
          summary.failed_files > 0
          || summary.remaining_diagnostics > 0
        then Error (Failure "Issues remain after tusk fix")
        else Ok ()
  in
  loop []

let run matches =
  match ArgParser.get_one matches "explain" with
  | Some code -> explain_code code
  | None ->
  let cwd =
    Env.current_dir ()
    |> Result.expect ~msg:"Failed to get current directory"
  in
  let scope = Fix_config.load_scope ~cwd in
  let mode =
    if ArgParser.get_flag matches "check" then
      Runner.Check
    else Runner.Apply
  in
  let format =
    match ArgParser.get_one matches "format" |> Option.unwrap_or ~default:"text" with
    | "json" -> Reporter.Json
    | _ -> Reporter.Text
  in
  let target = resolve_target matches in
  let files =
    resolve_files target
    |> List.filter (fun file -> not (Fix_config.should_ignore_file scope file))
  in
  if List.length files = 0 then (
    println "No OCaml files found.";
    Ok ())
  else
    run_with_coordinator ~format ~mode ~scope files

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help command;
      Error (Failure "Argument parsing failed")
  | Ok matches -> run matches
