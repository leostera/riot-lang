open Global

let init _total = ()
let on_result _idx _result = ()

let finalize (summary : Test_result.summary) =
  let open Data.Xml in
  let testcases =
    List.map
      (fun (r : Test_result.t) ->
        match r.result with
        | Test_result.Passed ->
            element "testcase" ~attrs:[ ("name", r.name) ] []
        | Test_result.Failed msg ->
            element "testcase"
              ~attrs:[ ("name", r.name) ]
              [ element "failure" ~attrs:[ ("message", msg) ] [] ]
        | Test_result.Skipped ->
            element "testcase"
              ~attrs:[ ("name", r.name) ]
              [ element "skipped" [] ])
      summary.results
  in

  let testsuite =
    element "testsuite"
      ~attrs:
        [
          ("tests", string_of_int summary.total);
          ("failures", string_of_int summary.failed);
          ("skipped", string_of_int summary.skipped);
        ]
      testcases
  in

  println "%s" declaration;
  println "%s" (to_string testsuite)
