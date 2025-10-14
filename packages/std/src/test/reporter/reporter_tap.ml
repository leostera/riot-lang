open Global

let init total =
  println "TAP version 14";
  println "1..%d" total

let on_result idx (result : Test_result.t) =
  match result.result with
  | Test_result.Passed -> println "ok %d - %s" idx result.name
  | Test_result.Failed msg ->
      println "not ok %d - %s" idx result.name;
      println "  ---";
      println "  message: '%s'" msg;
      println "  severity: fail";
      println "  ..."
  | Test_result.Skipped -> println "ok %d - %s # SKIP" idx result.name

let finalize _summary = ()
