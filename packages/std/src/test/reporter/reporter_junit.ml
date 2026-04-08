open Global
open Collections

let init = fun (_suite: Intf.suite_info) _total -> ()

let on_result = fun _idx _result -> ()

let finalize = fun (summary: Test_result.summary) ->
  let open Data.Xml in
    let testcases =
      List.map
        (fun (r: Test_result.t) ->
          let base_attrs = [ ("name", r.name) ] in
          let attrs =
            match r.test_type with
            | Test_case.UnitTest -> base_attrs
            | Test_case.Property { examples } -> base_attrs
            @ [ ("type", "property"); ("examples", string_of_int examples) ]
          in
          let attrs =
            match r.reliability with
            | Test_case.Stable -> attrs
            | Test_case.Flaky { retry_attempts } -> attrs
            @ [ ("reliability", "flaky"); ("retry_attempts", string_of_int retry_attempts) ]
          in
          match r.result with
          | Test_result.Passed -> element "testcase" ~attrs []
          | Test_result.Failed msg -> element
            "testcase"
            ~attrs
            [ element "failure" ~attrs:[ ("message", msg) ] [] ]
          | Test_result.Timed_out { timeout } -> element
            "testcase"
            ~attrs
            [
              element
                "failure"
                ~attrs:[
                  (
                    "message",
                    "timed out after " ^ string_of_int (Time.Duration.to_millis timeout) ^ "ms"
                  )
                ]
                [];
            ]
          | Test_result.Skipped -> element "testcase" ~attrs [ element "skipped" [] ])
        summary.results
    in
    let testsuite = element
      "testsuite"
      ~attrs:[
        ("tests", string_of_int summary.total);
        ("failures", string_of_int summary.failed);
        ("skipped", string_of_int summary.skipped);
      ]
      testcases in
    println declaration;
    println (to_string testsuite)
