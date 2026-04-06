open Std
module Test = Std.Test
module Test_selection = Riot_cli.Test_selection

let test_parse_request_keeps_global_query = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:(Some "hello")
      ~legacy_package:None
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  Test.assert_equal ~expected:None ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_uses_package_flag_for_narrowing = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:(Some "hello")
      ~legacy_package:(Some "std")
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  Test.assert_equal ~expected:(Some "std") ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_extracts_package_and_suite_selector = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:(Some "syn:diagnostic_tests")
      ~legacy_package:None
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  Test.assert_equal ~expected:(Some "syn") ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "diagnostic_tests") ~actual:request.suite_filter;
  Test.assert_equal ~expected:None ~actual:request.query;
  Ok ()

let test_parse_request_extracts_package_suite_and_query = fun _ctx ->
  let request = Test_selection.parse_request
    ~pattern:(Some "syn:diagnostic_tests:0001")
    ~legacy_package:None
    ~size_filter:Test_selection.All
    ~flaky_only:false in
  Test.assert_equal ~expected:(Some "syn") ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "diagnostic_tests") ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "0001") ~actual:request.query;
  Ok ()

let test_parse_request_preserves_raw_query_with_package_flag = fun _ctx ->
  let request = Test_selection.parse_request
    ~pattern:(Some "syn:diagnostic_tests")
    ~legacy_package:(Some "std")
    ~size_filter:Test_selection.All
    ~flaky_only:false in
  Test.assert_equal ~expected:(Some "std") ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "syn:diagnostic_tests") ~actual:request.query;
  Ok ()

let test_extra_args_omits_query_when_absent = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:None
      ~legacy_package:(Some "std")
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "--format"; "json" ] ~actual;
  Ok ()

let test_extra_args_prefixes_query_when_present = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:(Some "_long")
      ~legacy_package:None
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "_long"; "--format"; "json" ] ~actual;
  Ok ()

let test_extra_args_include_selection_flags = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:(Some "probe")
      ~legacy_package:None
      ~size_filter:Test_selection.Small
      ~flaky_only:true
  in
  let actual = Test_selection.extra_args request [] in
  Test.assert_equal ~expected:[ "probe"; "--small"; "--flaky" ] ~actual;
  Ok ()

let test_extra_args_include_policy_flags = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~pattern:None
      ~legacy_package:None
      ~size_filter:Test_selection.All
      ~flaky_only:false
  in
  let actual = Test_selection.extra_args
    ~small_test_timeout:(Some (Time.Duration.from_millis 500))
    ~flaky_max_retries:3
    request
    []
  in
  Test.assert_equal ~expected:[ "--small-timeout-ms"; "500"; "--flaky-max-retries"; "3" ] ~actual;
  Ok ()

let tests =
  Test.[
    case "test selection: keep global query" test_parse_request_keeps_global_query;
    case "test selection: use package flag for narrowing" test_parse_request_uses_package_flag_for_narrowing;
    case "test selection: extract package and suite selector" test_parse_request_extracts_package_and_suite_selector;
    case "test selection: extract package suite and query" test_parse_request_extracts_package_suite_and_query;
    case "test selection: preserve raw query with package flag" test_parse_request_preserves_raw_query_with_package_flag;
    case "test selection: omit query when absent" test_extra_args_omits_query_when_absent;
    case "test selection: prefix query when present" test_extra_args_prefixes_query_when_present;
    case "test selection: include selection flags" test_extra_args_include_selection_flags;
    case "test selection: include policy flags" test_extra_args_include_policy_flags;
  ]

let name = "Riot CLI Test Selection Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
