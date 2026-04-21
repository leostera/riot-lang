open Global
open Collections

type single_result =
  Passed
  | Failed of string
  | Timed_out of { timeout: Time.Duration.t }
  | Skipped

type t = {
  index: int;
  name: string;
  test_type: Test_case.test_type;
  size: Test_case.size;
  reliability: Test_case.reliability;
  attempts: int;
  result: single_result;
  duration: Time.Duration.t;
}

type summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  results: t list;
  duration: Time.Duration.t;
}

let make_summary = fun results ->
  let total = List.length results in
  let passed = List.filter results ~fn:(fun r -> r.result = Passed) |> List.length in
  let failed =
    List.filter results
      ~fn:(fun r ->
        match r.result with
        | Failed _
        | Timed_out _ -> true
        | _ -> false)
    |> List.length
  in
  let skipped = List.filter results ~fn:(fun r -> r.result = Skipped) |> List.length in
  let duration =
    List.fold_left results ~init:Time.Duration.zero
      ~fn:(fun acc (result: t) ->
        Time.Duration.add acc result.duration)
  in
  {
    total;
    passed;
    failed;
    skipped;
    results;
    duration;
  }
