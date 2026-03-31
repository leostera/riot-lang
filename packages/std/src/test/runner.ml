open Global
open Collections

type mode =
  Sequential
  | Shuffle

type target =
  All
  | FilterBySubstring of string

type config = {
  concurrency : int;
  reporter : (module Reporter.Intf);
  mode : mode;
  target : target;
  suite_info : Reporter.suite_info;
}

type run_summary = Test_result.summary

let filter_tests = fun target tests ->
  match target with
  | All -> tests
  | FilterBySubstring query ->
      List.filter
        (fun (test:Test_case.t) ->
          String.contains test.name query)
        tests

let shuffle_list = fun lst ->
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Kernel.Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let run_single_test = fun reporter index (test:Test_case.t) ->
  let name = test.name in
  let test_type = test.test_type in
  let result =
    if test.skip then
      Test_result.{index; name; test_type; result = Skipped}
    else
      match test.fn () with
      | exception exn ->
          let result =
            let exn = Exception.to_string exn in
            let bt = Exception.get_backtrace () in
            let reason = exn ^ "\n\n" ^ bt in
            Test_result.Failed reason
          in
          Test_result.{index; name; test_type; result}
      | Error msg ->
          Test_result.{index; name; test_type; result = Failed msg}
      | Ok () ->
          Test_result.{index; name; test_type; result = Passed}
  in
  let module R = (val reporter : Reporter.Intf) in
  R.on_result index result;
  result

let run_tests = fun ~config tests ->
  Exception.record_backtrace true;
  let filtered_tests = filter_tests config.target tests in
  let tests_to_run =
    match config.mode with
    | Sequential -> filtered_tests
    | Shuffle -> shuffle_list filtered_tests
  in
  let module R = (val config.reporter : Reporter.Intf) in
  R.init config.suite_info (List.length tests_to_run);
  let results =
    List.mapi (fun i test -> run_single_test config.reporter (i + 1) test) tests_to_run
  in
  let summary = Test_result.make_summary results in
  R.finalize summary;
  summary
