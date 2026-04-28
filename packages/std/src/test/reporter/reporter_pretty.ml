open Global
open Collections

let metadata_labels = fun size reliability ->
  let size_labels =
    match size with
    | Test_case.Small -> []
    | Test_case.Large -> [ "large" ]
  in
  let reliability_labels =
    match reliability with
    | Test_case.Stable -> []
    | Test_case.Flaky { retry_attempts } -> [ "flaky/" ^ Int.to_string retry_attempts ]
  in
  List.append size_labels reliability_labels

let metadata_suffix = fun size reliability ->
  match metadata_labels size reliability with
  | [] -> ""
  | labels -> " [" ^ String.concat " " labels ^ "]"

let attempts_suffix = fun attempts ->
  if attempts <= 1 then
    ""
  else
    " after " ^ Int.to_string attempts ^ " attempts"

let format_duration_us = fun duration_us ->
  if duration_us < 1_000 then
    Int.to_string duration_us ^ "µs"
  else if duration_us < 1_000_000 then
    Float.to_string ~precision:2 (Float.from_int duration_us /. 1_000.0) ^ "ms"
  else
    Float.to_string ~precision:2 (Float.from_int duration_us /. 1_000_000.0) ^ "s"

let duration_suffix = fun duration ->
  " (" ^ format_duration_us (Time.Duration.to_micros duration) ^ ")"

let init = fun (suite: Intf.suite_info) total ->
  (
    match (suite.source_file, suite.binary_path) with
    | (Some source, Some binary) ->
        println "";
        println ("     Running " ^ Path.to_string source ^ " (" ^ Path.to_string binary ^ ")")
    | (Some source, None) ->
        println "";
        println ("     Running " ^ Path.to_string source)
    | (None, _) -> ()
  );
  println "";
  println ("running " ^ Int.to_string total ^ " tests")

let on_result = fun _idx (result: Test_result.t) ->
  let prefix =
    match result.test_type with
    | Test_case.UnitTest -> "test"
    | Test_case.Property _ -> "prop"
  in
  let metadata = metadata_suffix result.size result.reliability in
  match result.result with
  | Test_result.Passed ->
      let suffix =
        match result.test_type with
        | Test_case.UnitTest -> "ok"
        | Test_case.Property { examples } -> Int.to_string examples ^ " examples ok"
      in
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... "
        ^ suffix
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.duration)
  | Test_result.Failed msg ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... FAILED"
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.duration);
      println ("       " ^ msg)
  | Test_result.Timed_out { timeout } ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... TIMED OUT after "
        ^ Int.to_string (Time.Duration.to_millis timeout)
        ^ "ms"
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.duration)
  | Test_result.Skipped ->
      println
        (prefix ^ " " ^ result.name ^ metadata ^ " ... skipped" ^ duration_suffix result.duration)

let finalize = fun (summary: Test_result.summary) ->
  println "";
  let status =
    if summary.failed > 0 then
      "FAILED"
    else
      "ok"
  in
  println
    ("test result: "
    ^ status
    ^ ". "
    ^ Int.to_string summary.passed
    ^ " passed; "
    ^ Int.to_string summary.failed
    ^ " failed; "
    ^ Int.to_string summary.skipped
    ^ " skipped")
