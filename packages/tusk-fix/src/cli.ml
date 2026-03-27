open Std

type run_outcome = {
  result : Runner.run_result;
  limit_reached : bool;
}

let command =
  let open ArgParser in
  let open Arg in
  command "fix"
  |> about "Lint OCaml code and optionally apply safe fixes"
  |> args
       [
         flag "list-rules" |> long "list-rules"
         |> help "List all available rules in the current tusk-fix runtime";
         flag "list-diagnostics" |> long "list-diagnostics"
         |> help "List all available diagnostics in the current tusk-fix runtime";
         flag "apply" |> long "apply"
         |> help "Apply safe fixes to files";
         flag "check" |> long "check"
         |> help
              "Check for issues without modifying files (default; kept for compatibility)";
         option "limit" |> long "limit"
         |> help "Stop after surfacing at most N diagnostics";
         option "explain" |> long "explain"
         |> help "Explain a rule id (e.g. riot:snake-case-type-names or std:no-stdlib)";
         flag "json" |> long "json"
         |> help "Emit machine-readable JSON output";
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
  if List.length grouped > 0 then
    print
      (Diagnostic.grouped_list_to_formatted_output ~file:result.file
         ~source:result.final_source grouped)

let print_parse_diagnostics result =
  if List.length result.Runner.parse_diagnostics > 0 then
    print
      (Syn.DiagnosticReporter.format ~file:(Path.to_string result.file)
         ~source:result.final_source result.parse_diagnostics)

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

let sort_file_results results =
  List.sort
    (fun left right ->
      String.compare
        (Path.to_string left.Runner.file)
        (Path.to_string right.Runner.file))
    results

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

let json_object_with_type type_name json =
  let open Data.Json in
  match json with
  | Object fields -> Object (("type", String type_name) :: fields)
  | _ -> panic "expected JSON object"

let start_event_to_json ~mode ~concurrency =
  let open Data.Json in
  Object
    [
      ("type", String "start");
      ("mode", String (match mode with Runner.Check -> "check" | Runner.Apply -> "apply"));
      ("concurrency", Int concurrency);
    ]

let file_event_to_json result =
  json_object_with_type "file" (Runner.file_result_to_json result)

let summary_event_to_json ~limit_reached summary =
  let open Data.Json in
  match Runner.summary_to_json summary with
  | Object fields ->
      Object
        (("type", String "summary") :: ("limit_reached", Bool limit_reached) :: fields)
  | _ -> panic "expected summary JSON object"

let print_json_event json =
  print (Data.Json.to_string json);
  print "\n"

let explain_rule rule_id =
  match Explanations.explain rule_id with
  | Some entry ->
      print (Explanations.format entry);
      Ok ()
  | None -> Error (Failure ("Unknown tusk-fix rule id: " ^ rule_id))

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

let display_rule_id_text rule_id =
  let package_name, local_id = split_rule_id rule_id in
  package_name ^ ":" ^ local_id

let display_rule_id rule =
  display_rule_id_text (Rule.id rule)

let sorted_rules () =
  Pipeline.default_rules ()
  |> List.sort (fun left right ->
         let left_package, left_local = split_rule_id (Rule.id left) in
         let right_package, right_local = split_rule_id (Rule.id right) in
         let package_cmp = compare_package_name left_package right_package in
         if package_cmp != 0 then
           package_cmp
         else if String.equal left_package "riot" then
           let left_category =
             Pipeline.builtin_rule_category left_local
             |> Option.unwrap_or ~default:"Other"
           in
           let right_category =
             Pipeline.builtin_rule_category right_local
             |> Option.unwrap_or ~default:"Other"
           in
           let category_cmp = String.compare left_category right_category in
           if category_cmp != 0 then
             category_cmp
           else
             String.compare left_local right_local
         else
           String.compare left_local right_local)

let sorted_diagnostics () =
  Explanations.all ()
  |> List.sort (fun left right ->
         String.compare
           (display_rule_id_text left.Explanation.rule_id)
           (display_rule_id_text right.Explanation.rule_id))

let rule_to_json rule =
  let open Data.Json in
  let package_name, local_id = split_rule_id (Rule.id rule) in
  Object
    [
      ("id", string (display_rule_id rule));
      ("local_id", string local_id);
      ("package", string package_name);
      ("category",
       (if String.equal package_name "riot" then
          match Pipeline.builtin_rule_category local_id with
          | Some category ->
              string category
          | None ->
              Null
        else
          Null));
      ("description", string (Rule.description rule));
      ("enabled", bool (Rule.enabled rule));
    ]

let diagnostic_to_json entry =
  let open Data.Json in
  Object
    [
      ("rule_id", string (display_rule_id_text entry.Explanation.rule_id));
      ("message", string entry.Explanation.message);
    ]

let list_rules_text rules =
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  let rec build_lines current_package current_category acc = function
    | [] -> List.rev acc
    | rule :: rest ->
        let package_name, local_id = split_rule_id (Rule.id rule) in
        let category =
          if String.equal package_name "riot" then
            Pipeline.builtin_rule_category local_id
          else
            None
        in
        let rule_line =
          if String.equal package_name "riot" then
            "  " ^ bold (display_rule_id rule) ^ " - " ^ Rule.description rule
          else
            bold (display_rule_id rule) ^ " - " ^ Rule.description rule
        in
        let acc =
          match package_name, current_package, category, current_category with
          | "riot", Some "riot", Some category_name, Some current
            when not (String.equal category_name current) ->
              rule_line :: ("  " ^ category_name ^ ":") :: acc
          | "riot", Some "riot", _, _ ->
              rule_line :: acc
          | "riot", _, Some category_name, _ ->
              rule_line :: ("  " ^ category_name ^ ":") :: "riot:" :: "" :: acc
          | _, Some current, _, _ when not (String.equal current package_name) ->
              rule_line :: (package_name ^ ":") :: "" :: acc
          | _, Some _, _, _ ->
              rule_line :: acc
          | _, None, _, _ ->
              rule_line :: (package_name ^ ":") :: acc
        in
        build_lines (Some package_name) category acc rest
  in
  build_lines None None [] rules |> String.concat "\n"

let list_diagnostics_text entries =
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  entries
  |> List.map (fun entry ->
         bold (display_rule_id_text entry.Explanation.rule_id)
         ^ " - " ^ entry.Explanation.message)
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
  let _coordinator =
    Coordinator.start
      { input = Coordinator.Files files; concurrency; limit; mode; scope; owner }
  in
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

let run_with_coordinator ~format ~mode ~scope ~limit ~roots () =
  let concurrency = recommended_concurrency ~limit in
  if format = Reporter.Text then
    println ("Scanning with " ^ Int.to_string concurrency ^ " workers...")
  else
    print_json_event (start_event_to_json ~mode ~concurrency);
  let outcome =
    let owner = self () in
    let _coordinator =
      Coordinator.start
        {
          input = Coordinator.Roots roots;
          concurrency;
          limit;
          mode;
          scope;
          owner;
        }
    in
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
          if format = Reporter.Json then
            print_json_event (file_event_to_json result);
          loop (result :: results_rev) diagnostics_seen
            (limit_reached || limit_reached_now)
      | `AllComplete _summary ->
          let files = List.rev results_rev in
          let summary = Runner.summarize files in
          {
            result = Runner.{ files; summary };
            limit_reached;
          }
    in
    loop [] 0 false
  in
  (match format with
  | Reporter.Json ->
      print_json_event
        (summary_event_to_json ~limit_reached:outcome.limit_reached
           outcome.result.summary)
  | Reporter.Text ->
      if outcome.result.summary.total_files = 0 then
        println "No OCaml files found."
      else (
        sort_file_results outcome.result.files
        |> List.iter (print_text_result mode);
        if outcome.limit_reached then (
          println "";
          println
            ("\027[1;33m!\027[0m Reached diagnostic limit "
           ^ (limit |> Option.map Int.to_string |> Option.unwrap_or ~default:"0")
           ^ "; stopped early"));
        print_text_summary mode outcome.result.summary));
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
  let apply = ArgParser.get_flag matches "apply" in
  let check = ArgParser.get_flag matches "check" in
  let format = if ArgParser.get_flag matches "json" then Reporter.Json else Reporter.Text in
  let limit =
    match ArgParser.get_int matches "limit" with
    | Some n when n > 0 -> Ok (Some n)
    | Some _ -> Error (Failure "--limit must be greater than 0")
    | None -> Ok None
  in
  match limit with
  | Error _ as err -> err
  | Ok limit ->
      if apply && check then
        Error (Failure "cannot use both --apply and --check")
      else (
        let mode =
          if apply then Runner.Apply else Runner.Check
        in
        match
          ArgParser.get_flag matches "list-rules",
          ArgParser.get_flag matches "list-diagnostics",
          ArgParser.get_one matches "explain"
        with
        | true, _, _ -> list_rules format
        | false, true, _ -> list_diagnostics format
        | false, false, Some code -> explain_rule code
        | false, false, None ->
            let target = resolve_target matches in
            run_with_coordinator ~format ~mode ~scope ~limit ~roots:[ target ] ())

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help command;
      Error (Failure "Argument parsing failed")
  | Ok matches -> run matches
