open Std

module Test = Std.Test
module Test_selection = Tusk_cli.Test_selection

let sample_suites =
  [
    Test_selection.
      {
        package_name = "std";
        suite_name = "std_test_cli_tests";
        case_names =
          [
            "list-tests lists all sample cases";
            "run-tests pattern matches suffix substring";
            "run-tests pattern matches middle substring";
          ];
      };
    Test_selection.
      {
        package_name = "tty";
        suite_name = "stdout_tests";
        case_names = [ "writes ansi output"; "handles resize" ];
      };
    Test_selection.
      {
        package_name = "foo";
        suite_name = "foo_tests";
        case_names = [ "alpha_long"; "beta" ];
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
  let selections =
    Test_selection.select (Test_selection.Query "std") sample_suites
  in
  match selections with
  | [
   Test_selection.RunSuite { package_name = "std"; suite_name = "std_test_cli_tests"; _ };
   Test_selection.RunSuite { package_name = "tty"; suite_name = "stdout_tests"; _ };
  ] ->
      Ok ()
  | _ -> Error "expected std query to run matching suites in full"

let test_select_filters_cases_for_case_only_matches () =
  let selections =
    Test_selection.select (Test_selection.Query "_long") sample_suites
  in
  match selections with
  | [
   Test_selection.RunCases
     {
       suite = { package_name = "foo"; suite_name = "foo_tests"; _ };
       query = "_long";
       matched_cases = [ "alpha_long" ];
     };
  ] ->
      Ok ()
  | _ -> Error "expected _long query to select only matching cases"

let test_select_respects_package_scoped_query () =
  let selections =
    Test_selection.select
      (Test_selection.PackageQuery { package_name = "std"; query = "list-tests" })
      sample_suites
  in
  match selections with
  | [
   Test_selection.RunCases
     {
       suite = { package_name = "std"; suite_name = "std_test_cli_tests"; _ };
       query = "list-tests";
       matched_cases = [ "list-tests lists all sample cases" ];
     };
  ] ->
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
      case "test selection: filter cases for case-only matches"
        test_select_filters_cases_for_case_only_matches;
      case "test selection: respect package-scoped query"
        test_select_respects_package_scoped_query;
    ]

let name = "Tusk CLI Test Selection Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
