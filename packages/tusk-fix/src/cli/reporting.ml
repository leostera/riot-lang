open Std

let print_diagnostics = fun result ->
  let grouped = Diagnostic.group_diagnostics result.Runner.diagnostics in
  if List.length grouped > 0 then
    print
      (Diagnostic.grouped_list_to_formatted_output ~file:result.file ~source:result.final_source grouped)

let print_parse_diagnostics = fun result ->
  if List.length result.Runner.parse_diagnostics > 0 then
    print
      (Syn.DiagnosticReporter.format
        ~file:(Path.to_string result.file)
        ~source:result.final_source
        result.parse_diagnostics)

let print_text_result = fun mode result ->
  let rel = Common.relative_to_cwd result.Runner.file in
  match result.error with
  | Some error ->
      println ("\027[1;31m✗\027[0m " ^ rel);
      println ("  " ^ error)
  | None ->
      if result.changed then
        println
          ("\027[1;32m✓\027[0m "
          ^ rel
          ^ " (applied "
          ^ Int.to_string (List.length result.applied_fixes)
          ^ " safe fixes)");
      if List.length result.parse_diagnostics > 0 then
        (
          println
            ("\027[1;31m✗\027[0m "
            ^ rel
            ^ " ("
            ^ Int.to_string (List.length result.parse_diagnostics)
            ^ " parse issues)");
          print_parse_diagnostics result
        );
      if List.length result.diagnostics > 0 then
        (
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
            (prefix ^ rel ^ " (" ^ Int.to_string (List.length result.diagnostics) ^ " " ^ suffix ^ ")");
          print_diagnostics result
        )

let print_text_summary = fun mode summary ->
  println "";
  match mode with
  | Runner.Check ->
      if summary.Runner.remaining_diagnostics = 0 && summary.failed_files = 0 then
        println
          ("\027[1;32m✓\027[0m No issues found in " ^ Int.to_string summary.total_files ^ " files")
      else
        println
          ("\027[1;31m✗\027[0m Found "
          ^ Int.to_string summary.remaining_diagnostics
          ^ " issues across "
          ^ Int.to_string summary.total_files
          ^ " files")
  | Runner.Apply ->
      if summary.remaining_diagnostics = 0 && summary.failed_files = 0 then
        println
          ("\027[1;32m✓\027[0m Applied "
          ^ Int.to_string summary.applied_fixes
          ^ " safe fixes across "
          ^ Int.to_string summary.changed_files
          ^ " files")
      else
        println
          ("\027[1;33m!\027[0m Applied "
          ^ Int.to_string summary.applied_fixes
          ^ " safe fixes across "
          ^ Int.to_string summary.changed_files
          ^ " files; "
          ^ Int.to_string summary.remaining_diagnostics
          ^ " issues remain")

let json_object_with_type = fun type_name json ->
  let open Data.Json in
    match json with
    | Object fields -> Object (("type", String type_name) :: fields)
    | _ -> panic "expected JSON object"

let timestamp_ms = fun () ->
  Time.SystemTime.now () |> Time.SystemTime.nanos |> Int64.div 1_000_000L |> Int64.to_int

let start_event_to_json = fun ~mode ~concurrency ->
  let open Data.Json in
    Object [ ("type", String "start"); (
        "mode",
        String (
          match mode with
          | Runner.Check -> "check"
          | Runner.Apply -> "apply"
        )
      ); ("concurrency", Int concurrency); ]

let file_started_event_to_json = fun file ->
  let open Data.Json in Object [
    ("type", String "file_started");
    ("file", String (Path.to_string file));
    ("timestamp_ms", Int (timestamp_ms ()));
  ]

let progress_event_to_json = fun file (event: Fixme.Source_runner.progress_event) ->
  let open Data.Json in
    let phase_fields =
      match event.phase with
      | Parsed { parse_diagnostics } -> [
        ("stage", String "parsed");
        ("parse_diagnostics", Int parse_diagnostics)
      ]
      | CstBuilt -> [ ("stage", String "cst_built") ]
      | RuleStarted { rule_id } -> [ ("stage", String "rule_started"); ("rule_id", String rule_id) ]
      | RuleFinished { rule_id; diagnostics } -> [
        ("stage", String "rule_finished");
        ("rule_id", String rule_id);
        ("diagnostics", Int diagnostics)
      ]
    in
    Object ([
      ("type", String "progress");
      ("file", String (Path.to_string file));
      ("timestamp_ms", Int event.timestamp_ms);
    ]
    @ phase_fields)

let file_event_to_json = fun result ->
  json_object_with_type "file" (Runner.file_result_to_json result)

let summary_event_to_json = fun ~limit_reached summary ->
  let open Data.Json in
    match Runner.summary_to_json summary with
    | Object fields -> Object (("type", String "summary")
    :: ("limit_reached", Bool limit_reached)
    :: fields)
    | _ -> panic "expected summary JSON object"

let print_json_event = fun json ->
  print (Data.Json.to_string json);
  print "\n"
