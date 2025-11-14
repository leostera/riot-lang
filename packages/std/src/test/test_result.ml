open Global
open Collections

type single_result = Passed | Failed of string | Skipped
type t = { index : int; name : string; test_type : Test_case.test_type; result : single_result }

type summary = {
  total : int;
  passed : int;
  failed : int;
  skipped : int;
  results : t list;
}

let make_summary results =
  let total = List.length results in
  let passed =
    List.filter (fun r -> r.result = Passed) results |> List.length
  in
  let failed =
    List.filter
      (fun r -> match r.result with Failed _ -> true | _ -> false)
      results
    |> List.length
  in
  let skipped =
    List.filter (fun r -> r.result = Skipped) results |> List.length
  in
  { total; passed; failed; skipped; results }
