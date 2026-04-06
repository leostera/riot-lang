open Global
open Collections

let suite_started_monotonic = ref None

let duration_us = fun duration -> Time.Duration.to_micros duration

let init = fun (_suite: Intf.suite_info) _total ->
  suite_started_monotonic := Some (Time.Instant.now ())

let on_result = fun _idx _result -> ()

let finalize = fun (summary: Test_result.summary) ->
  let open Data.Json in
    let suite_duration =
      match !suite_started_monotonic with
      | Some start -> Time.Instant.elapsed start
      | None -> summary.duration
    in
    let test_results =
      List.map
        (fun (r: Test_result.t) ->
          let type_fields =
            match r.test_type with
            | Test_case.UnitTest -> [ ("type", string "test") ]
            | Test_case.Property { examples } -> [
              ("type", string "property");
              ("examples", int examples)
            ]
          in
          let timing_fields =
            [ ("duration_us", int (duration_us r.duration)) ]
          in
          let base_fields =
            match r.result with
            | Test_result.Passed -> [ ("name", string r.name); ("status", string "passed") ]
            | Test_result.Failed msg -> [
              ("name", string r.name);
              ("status", string "failed");
              ("message", string msg);
            ]
            | Test_result.Skipped -> [ ("name", string r.name); ("status", string "skipped") ]
          in
          obj (base_fields @ type_fields @ timing_fields))
        summary.results
    in
    let summary_json = obj
      [
        ("total", int summary.total);
        ("passed", int summary.passed);
        ("failed", int summary.failed);
        ("skipped", int summary.skipped);
        ("duration_us", int (duration_us summary.duration));
      ] in
    let output = obj [
      ("tests", array test_results);
      ("summary", summary_json);
      ("started_at_us", int 0);
      ("completed_at_us", int (duration_us suite_duration));
      ("duration_us", int (duration_us suite_duration));
    ] in
    println (to_string output)
