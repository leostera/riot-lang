open Global

let init _total = ()
let on_result _idx _result = ()

let finalize (summary : Test_result.summary) =
  let open Data.Json in
  let test_results =
    List.map
      (fun (r : Test_result.t) ->
        match r.result with
        | Test_result.Passed ->
            obj [ ("name", string r.name); ("status", string "passed") ]
        | Test_result.Failed msg ->
            obj
              [
                ("name", string r.name);
                ("status", string "failed");
                ("message", string msg);
              ]
        | Test_result.Skipped ->
            obj [ ("name", string r.name); ("status", string "skipped") ])
      summary.results
  in

  let summary_json =
    obj
      [
        ("total", int summary.total);
        ("passed", int summary.passed);
        ("failed", int summary.failed);
        ("skipped", int summary.skipped);
      ]
  in

  let output =
    obj [ ("tests", array test_results); ("summary", summary_json) ]
  in

  println "%s" (to_string output)
