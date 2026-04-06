open Global
open Collections

type single_result =
  Passed
  | Failed of string
  | Skipped

type t = {
  index: int;
  name: string;
  test_type: Test_case.test_type;
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
  let passed = List.filter (fun r -> r.result = Passed) results |> List.length in
  let failed =
    List.filter
      (fun r ->
        match r.result with
        | Failed _ -> true
        | _ -> false)
      results
    |> List.length
  in
  let skipped = List.filter (fun r -> r.result = Skipped) results |> List.length in
  let duration =
    List.fold_left
      (fun acc (result: t) ->
        Time.Duration.add acc result.duration)
      Time.Duration.zero
      results
  in
  {
    total;
    passed;
    failed;
    skipped;
    results;
    duration;
  }
