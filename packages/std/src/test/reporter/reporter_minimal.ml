open Global
open Collections

let init = fun (_suite: Intf.suite_info) _total -> ()

let on_result = fun _idx (result: Test_result.t) ->
  match result.result with
  | Test_result.Passed -> print "."
  | Test_result.Failed _ -> print "F"
  | Test_result.Timed_out _ -> print "T"
  | Test_result.Skipped -> print "S"

let finalize = fun (summary: Test_result.summary) ->
  println "";
  println
    ("Tests: "
    ^ string_of_int summary.total
    ^ ", Passed: "
    ^ string_of_int summary.passed
    ^ ", Failed: "
    ^ string_of_int summary.failed
    ^ ", Skipped: "
    ^ string_of_int summary.skipped)
