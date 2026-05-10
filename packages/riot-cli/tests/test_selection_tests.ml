open Std

module Test = Std.Test
module Test_selection = Riot_cli.Test_selection

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let parse_cli = fun args ->
  match ArgParser.get_matches (Riot_cli.Cli.build_cli ()) args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_parse_request_keeps_global_query = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "hello")
      ~package_filters:[]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal ~expected:[] ~actual:request.package_filters;
  Test.assert_equal ~expected:None ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_uses_package_flag_for_narrowing = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "hello")
      ~package_filters:[ package_name "std" ]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal ~expected:[ package_name "std" ] ~actual:request.package_filters;
  Test.assert_equal ~expected:(Some (package_name "std")) ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_keeps_multiple_package_filters = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "hello")
      ~package_filters:[ package_name "std"; package_name "syn" ]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal
    ~expected:[ package_name "std"; package_name "syn" ]
    ~actual:request.package_filters;
  Test.assert_equal ~expected:None ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_extracts_package_and_suite_selector = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "syn:diagnostic_tests")
      ~package_filters:[]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal ~expected:[ package_name "syn" ] ~actual:request.package_filters;
  Test.assert_equal ~expected:(Some (package_name "syn")) ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "diagnostic_tests") ~actual:request.suite_filter;
  Test.assert_equal ~expected:None ~actual:request.query;
  Ok ()

let test_parse_request_extracts_package_suite_and_query = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "syn:diagnostic_tests:0001")
      ~package_filters:[]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal ~expected:[ package_name "syn" ] ~actual:request.package_filters;
  Test.assert_equal ~expected:(Some (package_name "syn")) ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "diagnostic_tests") ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "0001") ~actual:request.query;
  Ok ()

let test_parse_request_preserves_raw_query_with_package_flag = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "syn:diagnostic_tests")
      ~package_filters:[ package_name "std" ]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  Test.assert_equal ~expected:[ package_name "std" ] ~actual:request.package_filters;
  Test.assert_equal ~expected:(Some (package_name "std")) ~actual:request.package_filter;
  Test.assert_equal ~expected:None ~actual:request.suite_filter;
  Test.assert_equal ~expected:(Some "syn:diagnostic_tests") ~actual:request.query;
  Ok ()

let test_extra_args_omits_query_when_absent = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:None
      ~package_filters:[ package_name "std" ]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "--format"; "json" ] ~actual;
  Ok ()

let test_extra_args_prefixes_query_when_present = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "_long")
      ~package_filters:[]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "_long"; "--format"; "json" ] ~actual;
  Ok ()

let test_extra_args_include_selection_flags = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:(Some "probe")
      ~package_filters:[]
      ~size_filter:Test_selection.Small
      ~flaky_only:true
    |> Result.expect ~msg:"parse request failed"
  in
  let actual = Test_selection.extra_args request [] in
  Test.assert_equal ~expected:[ "probe"; "--small"; "--flaky" ] ~actual;
  Ok ()

let test_extra_args_include_policy_flags = fun _ctx ->
  let request =
    Test_selection.parse_request
      ~filter:None
      ~package_filters:[]
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.expect ~msg:"parse request failed"
  in
  let actual =
    Test_selection.extra_args
      ~small_test_timeout:(Some (Time.Duration.from_millis 500))
      ~flaky_max_retries:3
      request
      []
  in
  Test.assert_equal ~expected:[ "--small-timeout-ms"; "500"; "--flaky-max-retries"; "3"; ] ~actual;
  Ok ()

let test_test_command_parses_repeated_packages_and_filter = fun _ctx ->
  match parse_cli [ "riot"; "test"; "-p"; "std"; "-p"; "syn"; "-f"; "probe"; ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("test", test_matches) ->
          Test.assert_equal
            ~expected:[ "std"; "syn" ]
            ~actual:(ArgParser.get_many test_matches "package");
          Test.assert_equal
            ~expected:(Some "probe")
            ~actual:(ArgParser.get_one test_matches "filter");
          Ok ()
      | Some (name, _) -> Error ("expected test command, got: " ^ name)
      | None -> Error "expected top-level subcommand"

let test_bench_command_parses_repeated_packages_and_filter = fun _ctx ->
  match parse_cli
    [
      "riot";
      "bench";
      "-p";
      "std";
      "-p";
      "syn";
      "-f";
      "probe";
      "--compare";
      "3";
      "--iterations";
      "500";
      "--warmup";
      "25";
    ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("bench", bench_matches) ->
          Test.assert_equal
            ~expected:[ "std"; "syn" ]
            ~actual:(ArgParser.get_many bench_matches "package");
          Test.assert_equal
            ~expected:(Some "probe")
            ~actual:(ArgParser.get_one bench_matches "filter");
          Test.assert_equal ~expected:(Some 3) ~actual:(ArgParser.get_int bench_matches "compare");
          Test.assert_equal
            ~expected:(Some 500)
            ~actual:(ArgParser.get_int bench_matches "iterations");
          Test.assert_equal ~expected:(Some 25) ~actual:(ArgParser.get_int bench_matches "warmup");
          Ok ()
      | Some (name, _) -> Error ("expected bench command, got: " ^ name)
      | None -> Error "expected top-level subcommand"

let tests =
  Test.[
    case "test selection: keep global query" test_parse_request_keeps_global_query;
    case
      "test selection: use package flag for narrowing"
      test_parse_request_uses_package_flag_for_narrowing;
    case
      "test selection: keep multiple package filters"
      test_parse_request_keeps_multiple_package_filters;
    case
      "test selection: extract package and suite selector"
      test_parse_request_extracts_package_and_suite_selector;
    case
      "test selection: extract package suite and query"
      test_parse_request_extracts_package_suite_and_query;
    case
      "test selection: preserve raw query with package flag"
      test_parse_request_preserves_raw_query_with_package_flag;
    case "test selection: omit query when absent" test_extra_args_omits_query_when_absent;
    case "test selection: prefix query when present" test_extra_args_prefixes_query_when_present;
    case "test selection: include selection flags" test_extra_args_include_selection_flags;
    case "test selection: include policy flags" test_extra_args_include_policy_flags;
    case
      "cli: test parses repeated packages and filter"
      test_test_command_parses_repeated_packages_and_filter;
    case
      "cli: bench parses repeated packages and filter"
      test_bench_command_parses_repeated_packages_and_filter;
  ]

let name = "Riot CLI Test Selection Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
