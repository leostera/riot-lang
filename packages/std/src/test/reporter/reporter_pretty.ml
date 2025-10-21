open Global

let init (suite : Intf.suite_info) total =
  (match (suite.source_file, suite.binary_path) with
  | Some source, Some binary ->
      println "";
      println "     Running %s (%s)" source binary
  | Some source, None ->
      println "";
      println "     Running %s" source
  | None, _ -> ());
  println "";
  println "running %d tests" total

let on_result _idx (result : Test_result.t) =
  match result.result with
  | Test_result.Passed -> println "test %s ... ok" result.name
  | Test_result.Failed msg ->
      println "test %s ... FAILED" result.name;
      println "       %s" msg
  | Test_result.Skipped -> println "test %s ... skipped" result.name

let finalize (summary : Test_result.summary) =
  println "";
  println "test result: %s. %d passed; %d failed; %d skipped"
    (if summary.failed > 0 then "FAILED" else "ok")
    summary.passed summary.failed summary.skipped
