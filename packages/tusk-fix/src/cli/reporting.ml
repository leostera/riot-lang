open Std

let print_diagnostics = fun result ->
  let grouped = Diagnostic.group_diagnostics Runner.(result.diagnostics) in
  if not (List.is_empty grouped) then
    print
      (Diagnostic.grouped_list_to_formatted_output ~file:result.file ~source:result.final_source grouped)

let print_parse_diagnostics = fun result ->
  if not (List.is_empty Runner.(result.parse_diagnostics)) then
    print
      (Syn.DiagnosticReporter.format
        ~file:(Path.to_string result.file)
        ~source:result.final_source
        result.parse_diagnostics)

let print_text_result = fun mode result ->
  let rel = Common.relative_to_cwd Runner.(result.file) in
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
      if not (List.is_empty result.parse_diagnostics) then
        (
          println
            ("\027[1;31m✗\027[0m "
            ^ rel
            ^ " ("
            ^ Int.to_string (List.length result.parse_diagnostics)
            ^ " parse issues)");
          print_parse_diagnostics result
        );
      if not (List.is_empty result.diagnostics) then
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
      if Runner.(summary.remaining_diagnostics = 0) && summary.failed_files = 0 then
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

let print_json_event = fun json ->
  print (Data.Json.to_string json);
  print "\n"
