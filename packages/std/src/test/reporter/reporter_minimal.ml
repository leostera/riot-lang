open Global

let init (_suite : Intf.suite_info) _total = ()

let on_result _idx (result : Test_result.t) =
  match result.result with
  | Test_result.Passed -> print "."
  | Test_result.Failed _ -> print "F"
  | Test_result.Skipped -> print "S"

let finalize (summary : Test_result.summary) =
  println "";
  println "Tests: %d, Passed: %d, Failed: %d, Skipped: %d" summary.total
    summary.passed summary.failed summary.skipped
