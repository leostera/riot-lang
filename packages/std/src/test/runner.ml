open Global

type mode = Sequential | Shuffle
type target = All | FilterByPrefix of string

type config = {
  concurrency : int;
  reporter : (module Reporter.Intf);
  mode : mode;
  target : target;
}

type run_summary = Test_result.summary

let filter_tests target tests =
  match target with
  | All -> tests
  | FilterByPrefix prefix ->
      List.filter
        (fun (test : Test_case.t) -> String.starts_with ~prefix test.name)
        tests

let shuffle_list lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let run_single_test reporter idx (test : Test_case.t) =
  let result =
    if test.skip then
      {
        Test_result.index = idx;
        name = test.name;
        result = Test_result.Skipped;
      }
    else
      match test.fn () with
      | Ok () ->
          {
            Test_result.index = idx;
            name = test.name;
            result = Test_result.Passed;
          }
      | Error msg ->
          {
            Test_result.index = idx;
            name = test.name;
            result = Test_result.Failed msg;
          }
  in
  let module R = (val reporter : Reporter.Intf) in
  R.on_result idx result;
  result

let run_tests ~config tests =
  let filtered_tests = filter_tests config.target tests in
  let tests_to_run =
    match config.mode with
    | Sequential -> filtered_tests
    | Shuffle -> shuffle_list filtered_tests
  in

  let module R = (val config.reporter : Reporter.Intf) in
  R.init (List.length tests_to_run);

  let results =
    List.mapi
      (fun i test -> run_single_test config.reporter (i + 1) test)
      tests_to_run
  in
  let summary = Test_result.make_summary results in

  R.finalize summary;

  summary
