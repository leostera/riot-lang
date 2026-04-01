open Global
open Collections

let init = fun (suite: Intf.suite_info) total ->
  (
    match (suite.source_file, suite.binary_path) with
    | Some source, Some binary ->
        println "";
        println ("     Running " ^ source ^ " (" ^ binary ^ ")")
    | Some source, None ->
        println "";
        println ("     Running " ^ source)
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
  match result.result with
  | Test_result.Passed ->
      let suffix =
        match result.test_type with
        | Test_case.UnitTest -> "ok"
        | Test_case.Property { examples } -> Int.to_string examples ^ " examples ok"
      in
      println (prefix ^ " " ^ result.name ^ " ... " ^ suffix)
  | Test_result.Failed msg ->
      println (prefix ^ " " ^ result.name ^ " ... FAILED");
      println ("       " ^ msg)
  | Test_result.Skipped ->
      println (prefix ^ " " ^ result.name ^ " ... skipped")

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
