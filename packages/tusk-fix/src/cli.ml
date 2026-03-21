open Std

type run_outcome = {
  result : Runner.run_result;
  limit_reached : bool;
}

let command =
  let open ArgParser in
  let open Arg in
  command "fix"
  |> about "Lint OCaml code and apply safe fixes"
  |> args
       [
         flag "list-rules" |> long "list-rules"
         |> help "List all available rules in the current tusk-fix runtime";
         flag "list-diagnostics" |> long "list-diagnostics"
         |> help "List all available diagnostic codes in the current tusk-fix runtime";
         flag "check" |> long "check"
         |> help "Check for fixable issues without modifying files";
         option "limit" |> long "limit"
         |> help "Stop after surfacing at most N diagnostics";
         option "explain" |> long "explain"
         |> help "Explain a diagnostic code (e.g. F0001 or std:f0001)";
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

let diagnostic_count result =
  List.length result.Runner.parse_diagnostics + List.length result.diagnostics

let rec take n xs =
  if n <= 0 then
    []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let clip_result_to_limit remaining result =
  if remaining <= 0 then
    {
      result with
      Runner.parse_diagnostics = [];
      diagnostics = [];
    }
  else
    let parse_count = List.length result.Runner.parse_diagnostics in
    if parse_count >= remaining then
      {
        result with
        Runner.parse_diagnostics = take remaining result.parse_diagnostics;
        diagnostics = [];
      }
    else
      {
        result with
        Runner.diagnostics = take (remaining - parse_count) result.diagnostics;
      }

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
  match Explanations.explain code with
  | Some entry ->
      print (Explanations.format entry);
      Ok ()
  | None -> Error (Failure ("Unknown tusk-fix diagnostic code: " ^ code))

let split_rule_id rule_id =
  match String.index_opt rule_id ':' with
  | Some idx ->
      let package_name = String.sub rule_id 0 idx in
      let local_id =
        String.sub rule_id (idx + 1) (String.length rule_id - idx - 1)
      in
      package_name, local_id
  | None -> "riot", rule_id

let compare_package_name left right =
  match left = "riot", right = "riot" with
  | true, true | false, false -> String.compare left right
  | true, false -> -1
  | false, true -> 1

let display_rule_id rule =
  let package_name, local_id = split_rule_id (Rule.id rule) in
  package_name ^ ":" ^ local_id

let sorted_rules () =
  Pipeline.default_rules ()
  |> List.sort (fun left right ->
         let left_package, left_local = split_rule_id (Rule.id left) in
         let right_package, right_local = split_rule_id (Rule.id right) in
         let package_cmp = compare_package_name left_package right_package in
         if package_cmp != 0 then
           package_cmp
         else
           String.compare left_local right_local)

let sorted_diagnostics () =
  Explanations.all ()
  |> List.sort (fun left right ->
         let code_cmp =
           String.compare
             (String.lowercase_ascii left.Explanation.code)
             (String.lowercase_ascii right.Explanation.code)
         in
         if code_cmp != 0 then
           code_cmp
         else
           String.compare left.rule_id right.rule_id)

let rule_to_json rule =
  let open Data.Json in
  Object
    [
      ("id", string (Rule.id rule));
      ("description", string (Rule.description rule));
      ("enabled", bool (Rule.enabled rule));
    ]

let diagnostic_to_json entry =
  let open Data.Json in
  Object
    [
      ("code", string entry.Explanation.code);
      ("rule_id", string entry.rule_id);
      ("message", string entry.message);
    ]

let list_rules_text rules =
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  let rec build_lines current_package acc = function
    | [] -> List.rev acc
    | rule :: rest ->
        let package_name, _ = split_rule_id (Rule.id rule) in
        let acc =
          match current_package with
          | Some current when not (String.equal current package_name) ->
              ""
              :: (bold (display_rule_id rule) ^ " - " ^ Rule.description rule)
              :: acc
          | _ ->
              (bold (display_rule_id rule) ^ " - " ^ Rule.description rule)
              :: acc
        in
        build_lines (Some package_name) acc rest
  in
  build_lines None [] rules |> String.concat "\n"

let list_diagnostics_text entries =
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  entries
  |> List.map (fun entry ->
         bold (String.lowercase_ascii entry.Explanation.code)
         ^ " (" ^ entry.rule_id ^ ") - " ^ entry.message)
  |> String.concat "\n"

let list_rules_output ~format =
  let rules = sorted_rules () in
  match format with
  | Reporter.Text -> list_rules_text rules
  | Reporter.Json ->
      Data.Json.Array (List.map rule_to_json rules)
      |> Data.Json.to_string

let list_diagnostics_output ~format =
  let entries = sorted_diagnostics () in
  match format with
  | Reporter.Text -> list_diagnostics_text entries
  | Reporter.Json ->
      Data.Json.Array (List.map diagnostic_to_json entries)
      |> Data.Json.to_string

let list_rules format =
  print (list_rules_output ~format);
  if format = Reporter.Text then print "\n";
  Ok ()

let list_diagnostics format =
  print (list_diagnostics_output ~format);
  if format = Reporter.Text then print "\n";
  Ok ()

let recommended_concurrency ~limit =
  let concurrency =
    let recommended = System.available_parallelism in
    if recommended <= 0 then 1 else recommended
  in
  match limit with
  | Some max_diagnostics -> Int.min concurrency max_diagnostics
  | None -> concurrency

let run_result_with ~on_result ~mode ~scope ~limit ~files =
  let concurrency = recommended_concurrency ~limit in
  let owner = self () in
  let coordinator = Coordinator.start { files; concurrency; limit; mode; scope; owner } in
  let rec loop results_rev diagnostics_seen limit_reached =
    let selector = function
      | Messages.FileResult result -> `select (`FileResult result)
      | Messages.AllComplete summary -> `select (`AllComplete summary)
      | _ -> `skip
    in
    match receive ~selector () with
    | `FileResult { Messages.result; _ } ->
        let remaining_budget =
          match limit with
          | None -> None
          | Some max_diagnostics -> Some (max_diagnostics - diagnostics_seen)
        in
        let result =
          match remaining_budget with
          | None -> result
          | Some remaining -> clip_result_to_limit remaining result
        in
        let diagnostics_seen = diagnostics_seen + diagnostic_count result in
        let limit_reached_now =
          match limit with
          | Some max_diagnostics when diagnostics_seen >= max_diagnostics -> true
          | _ -> false
        in
        on_result result;
        loop (result :: results_rev) diagnostics_seen (limit_reached || limit_reached_now)
    | `AllComplete _summary ->
        let files = List.rev results_rev in
        let summary = Runner.summarize files in
        {
          result = Runner.{ files; summary };
          limit_reached;
        }
  in
  loop [] 0 false

let run_result ~mode ~scope ~limit ~files =
  run_result_with ~mode ~scope ~limit ~files ~on_result:(fun _ -> ())

let run_with_coordinator ~format ~mode ~scope ~limit files =
  let concurrency = recommended_concurrency ~limit in
  if format = Reporter.Text then
    println
      ("Scanning " ^ Int.to_string (List.length files) ^ " files with "
     ^ Int.to_string concurrency ^ " workers...");
  let outcome =
    run_result_with ~mode ~scope ~limit ~files ~on_result:(fun result ->
        match format with
        | Reporter.Text -> print_text_result mode result
        | Reporter.Json -> ())
  in
  (match format with
  | Reporter.Json ->
      print (Data.Json.to_string (Runner.run_result_to_json outcome.result));
      print "\n"
  | Reporter.Text ->
      if outcome.limit_reached then (
        println "";
        println
          ("\027[1;33m!\027[0m Reached diagnostic limit "
         ^ (limit |> Option.map Int.to_string |> Option.unwrap_or ~default:"0")
         ^ "; stopped early"));
      print_text_summary mode outcome.result.summary);
  if
    outcome.result.summary.failed_files > 0
    || outcome.result.summary.remaining_diagnostics > 0
  then Error (Failure "Issues remain after tusk fix")
  else Ok ()

let run matches =
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
  let limit =
    match ArgParser.get_int matches "limit" with
    | Some n when n > 0 -> Ok (Some n)
    | Some _ -> Error (Failure "--limit must be greater than 0")
    | None -> Ok None
  in
  match limit with
  | Error _ as err -> err
  | Ok limit ->
      match
        ArgParser.get_flag matches "list-rules",
        ArgParser.get_flag matches "list-diagnostics",
        ArgParser.get_one matches "explain"
      with
      | true, _, _ -> list_rules format
      | false, true, _ -> list_diagnostics format
      | false, false, Some code -> explain_code code
      | false, false, None ->
          let target = resolve_target matches in
          let files =
            resolve_files target
            |> List.filter (fun file -> not (Fix_config.should_ignore_file scope file))
          in
          if List.length files = 0 then (
            println "No OCaml files found.";
            Ok ())
          else
            run_with_coordinator ~format ~mode ~scope ~limit files

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help command;
      Error (Failure "Argument parsing failed")
  | Ok matches -> run matches
