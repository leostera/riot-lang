open Global

let init _total = ()

let on_result _idx (result : Test_result.t) =
  match result.result with
  | Test_result.Passed -> println "[ OK ] %s" result.name
  | Test_result.Failed msg ->
      println "[FAIL] %s" result.name;
      println "       %s" msg
  | Test_result.Skipped -> println "[SKIP] %s" result.name

let finalize (summary : Test_result.summary) =
  println "";
  println "Tests: %d, Passed: %d, Failed: %d, Skipped: %d" summary.total
    summary.passed summary.failed summary.skipped
