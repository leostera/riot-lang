open Global
open Collections

let init (_suite : Intf.suite_info) total =
  println "TAP version 14";
  println ("1.." ^ string_of_int total)

let on_result idx (result : Test_result.t) =
  let idx_str = string_of_int idx in
  match result.result with
  | Test_result.Passed -> println ("ok " ^ idx_str ^ " - " ^ result.name)
  | Test_result.Failed msg ->
      println ("not ok " ^ idx_str ^ " - " ^ result.name);
      println "  ---";
      println ("  message: '" ^ msg ^ "'");
      println "  severity: fail";
      println "  ..."
  | Test_result.Skipped -> println ("ok " ^ idx_str ^ " - " ^ result.name ^ " # SKIP")

let finalize _summary = ()
