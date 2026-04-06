open Global
open Collections

let size_label = function
  | Test_case.Small -> "small"
  | Test_case.Long -> "long"

let reliability_label = function
  | Test_case.Stable -> ""
  | Test_case.Flaky { retry_attempts } -> " flaky/" ^ Int.to_string retry_attempts

let attempts_suffix = fun attempts ->
  if attempts <= 1 then
    ""
  else
    " after " ^ Int.to_string attempts ^ " attempts"

let init = fun (suite: Intf.suite_info) total ->
  (
    match (suite.source_file, suite.binary_path) with
    | Some source, Some binary ->
        println "";
        println ("     Running " ^ Path.to_string source ^ " (" ^ Path.to_string binary ^ ")")
    | Some source, None ->
        println "";
        println ("     Running " ^ Path.to_string source)
    | None, _ ->
        ()
  );
  println "";
  println ("running " ^ string_of_int total ^ " tests")

let on_result = fun _idx (result: Test_result.t) ->
  let prefix =
    match result.test_type with
    | Test_case.UnitTest -> "test"
    | Test_case.Property _ -> "prop"
  in
  let metadata =
    " [" ^ size_label result.size ^ reliability_label result.reliability ^ "]"
  in
  match result.result with
  | Test_result.Passed ->
      let suffix =
        match result.test_type with
        | Test_case.UnitTest -> "ok"
        | Test_case.Property { examples } -> Int.to_string examples ^ " examples ok"
      in
      println (prefix ^ " " ^ result.name ^ metadata ^ " ... " ^ suffix ^ attempts_suffix result.attempts)
  | Test_result.Failed msg ->
      println (prefix ^ " " ^ result.name ^ metadata ^ " ... FAILED" ^ attempts_suffix result.attempts);
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
        ^ attempts_suffix result.attempts)
  | Test_result.Skipped ->
      println (prefix ^ " " ^ result.name ^ metadata ^ " ... skipped")

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
    ^ string_of_int summary.passed
    ^ " passed; "
    ^ string_of_int summary.failed
    ^ " failed; "
    ^ string_of_int summary.skipped
    ^ " skipped")
