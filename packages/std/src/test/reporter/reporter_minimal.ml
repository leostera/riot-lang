open Global
open Collections

let init = fun (_suite: Intf.suite_info) _total -> ()

let on_result = fun _idx (result: Test_result.t) ->
  match result.result with
  | Test_result.Passed -> print "."
  | Test_result.Failed _ -> print "F"
  | Test_result.Timed_out _ -> print "T"
  | Test_result.Skipped -> print "S"

let warn = fun message -> eprintln ("warning: " ^ message)

let finalize = fun (summary: Test_result.summary) ->
  println "";
  println
    ("Tests: "
    ^ Int.to_string summary.total
    ^ ", Passed: "
    ^ Int.to_string summary.passed
    ^ ", Failed: "
    ^ Int.to_string summary.failed
    ^ ", Skipped: "
    ^ Int.to_string summary.skipped)
