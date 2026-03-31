open Global
open Collections

let init = fun (_suite:Intf.suite_info) total ->
  println "TAP version 14";
  println ("1.." ^ string_of_int total)

let on_result = fun idx (result:Test_result.t) ->
  let idx_str = string_of_int idx in
  let name_with_type =
    match result.test_type with
    | Test_case.UnitTest -> result.name
    | Test_case.Property { examples } -> result.name ^ " (" ^ Int.to_string examples ^ " examples)"
  in
  match result.result with
  | Test_result.Passed ->
      println ("ok " ^ idx_str ^ " - " ^ name_with_type)
  | Test_result.Failed msg ->
      println ("not ok " ^ idx_str ^ " - " ^ name_with_type);
      println "  ---";
      println ("  message: '" ^ msg ^ "'");
      println "  severity: fail";
      println "  ..."
  | Test_result.Skipped ->
      println ("ok " ^ idx_str ^ " - " ^ name_with_type ^ " # SKIP")

let finalize = fun _summary -> ()
