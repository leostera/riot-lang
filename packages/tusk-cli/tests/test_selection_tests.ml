open Std

module Test = Std.Test
module Test_selection = Tusk_cli.Test_selection

let sample_suites =
  [
    Test_selection.
      {
        package_name = "std";
        suite_name = "std_test_cli_tests";
      };
    Test_selection.
      {
        package_name = "tty";
        suite_name = "stdout_tests";
      };
    Test_selection.
      {
        package_name = "foo";
        suite_name = "foo_tests";
      };
  ]

let test_parse_request_keeps_global_substring_queries () =
  match Test_selection.parse_request ~pattern:(Some "std") ~legacy_package:None with
  | Test_selection.Query "std" -> Ok ()
  | _ -> Error "expected plain pattern to be parsed as a global query"

let test_parse_request_supports_package_wildcard () =
  match
    Test_selection.parse_request ~pattern:(Some "std:...") ~legacy_package:None
  with
  | Test_selection.PackageAll "std" -> Ok ()
  | _ -> Error "expected pkg:... to be parsed as PackageAll"

let test_parse_request_supports_package_scoped_query () =
  match
    Test_selection.parse_request ~pattern:(Some "std:list-tests")
      ~legacy_package:None
  with
  | Test_selection.PackageQuery { package_name = "std"; query = "list-tests" } ->
      Ok ()
  | _ -> Error "expected pkg:query to be parsed as PackageQuery"

let test_select_runs_full_suite_for_package_and_suite_matches () =
  let std_execution =
    Test_selection.execution_for_suite (Test_selection.Query "std")
      (List.nth sample_suites 0)
  in
  let tty_execution =
    Test_selection.execution_for_suite (Test_selection.Query "std")
      (List.nth sample_suites 1)
  in
  match (std_execution, tty_execution) with
  | (Some Test_selection.RunSuite, Some Test_selection.RunSuite) ->
      Ok ()
  | _ -> Error "expected std query to run matching suites in full"

let test_select_runs_filtered_query_for_case_matches () =
  let execution =
    Test_selection.execution_for_suite (Test_selection.Query "_long")
      (List.nth sample_suites 2)
  in
  match execution with
  | Some (Test_selection.RunQuery "_long") ->
      Ok ()
  | _ -> Error "expected _long query to delegate to suite-level filtering"

let test_select_respects_package_scoped_query () =
  let std_execution =
    Test_selection.execution_for_suite
      (Test_selection.PackageQuery { package_name = "std"; query = "list-tests" })
      (List.nth sample_suites 0)
  in
  let foo_execution =
    Test_selection.execution_for_suite
      (Test_selection.PackageQuery { package_name = "std"; query = "list-tests" })
      (List.nth sample_suites 2)
  in
  match (std_execution, foo_execution) with
  | (Some (Test_selection.RunQuery "list-tests"), None) ->
      Ok ()
  | _ -> Error "expected package-scoped query to stay inside the package"

let tests =
  Test.
    [
      case "test selection: parse plain pattern as global query"
        test_parse_request_keeps_global_substring_queries;
      case "test selection: parse package wildcard"
        test_parse_request_supports_package_wildcard;
      case "test selection: parse package scoped query"
        test_parse_request_supports_package_scoped_query;
      case "test selection: run full suite for package and suite matches"
        test_select_runs_full_suite_for_package_and_suite_matches;
      case "test selection: delegate case-only matches to suite queries"
        test_select_runs_filtered_query_for_case_matches;
      case "test selection: respect package-scoped query"
        test_select_respects_package_scoped_query;
    ]

let name = "Tusk CLI Test Selection Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
