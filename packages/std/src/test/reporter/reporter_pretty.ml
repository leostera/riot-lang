open Global
open Collections

let init (suite : Intf.suite_info) total =
  (match (suite.source_file, suite.binary_path) with
  | Some source, Some binary ->
      println "";
      println ("     Running " ^ source ^ " (" ^ binary ^ ")")
  | Some source, None ->
      println "";
      println ("     Running " ^ source)
  | None, _ -> ());
  println "";
  println ("running " ^ string_of_int total ^ " tests")

let on_result _idx (result : Test_result.t) =
  match result.result with
  | Test_result.Passed -> println ("test " ^ result.name ^ " ... ok")
  | Test_result.Failed msg ->
      println ("test " ^ result.name ^ " ... FAILED");
      println ("       " ^ msg)
  | Test_result.Skipped -> println ("test " ^ result.name ^ " ... skipped")

let finalize (summary : Test_result.summary) =
  println "";
  let status = if summary.failed > 0 then "FAILED" else "ok" in
  println ("test result: " ^ status ^ ". " ^ 
           string_of_int summary.passed ^ " passed; " ^
           string_of_int summary.failed ^ " failed; " ^
           string_of_int summary.skipped ^ " skipped")
