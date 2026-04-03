open Std
module Test = Std.Test

let make_workspace = fun binaries ->
  let package = Riot_model.Package.make
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~relative_path:(Path.v "packages/demo")
    ~binaries
    () in
  Riot_model.Workspace.make ~root:(Path.v "/workspace") ~packages:[ package ] ()

let test_collect_test_suites_filters_workspace_binaries = fun _ctx ->
  let workspace = make_workspace
    [
      Riot_model.Package.{ name = "alpha_tests"; path = Path.v "tests/alpha_tests.ml" };
      Riot_model.Package.{ name = "main"; path = Path.v "src/main.ml" };
      Riot_model.Package.{ name = "beta-tests"; path = Path.v "tests/beta-tests.ml" };
    ] in
  let actual = Riot_build.collect_test_suites workspace () in
  Test.assert_equal
    ~expected:[
      Riot_build.{ package_name = "demo"; suite_name = "alpha_tests" };
      Riot_build.{ package_name = "demo"; suite_name = "beta-tests" };
    ]
    ~actual;
  Ok ()

let test_collect_test_suites_filters_by_suite_name = fun _ctx ->
  let workspace = make_workspace
    [
      Riot_model.Package.{ name = "alpha_tests"; path = Path.v "tests/alpha_tests.ml" };
      Riot_model.Package.{ name = "beta-tests"; path = Path.v "tests/beta-tests.ml" };
    ] in
  let actual = Riot_build.collect_test_suites workspace ~suite_filter:"beta-tests" () in
  Test.assert_equal ~expected:[ Riot_build.{ package_name = "demo"; suite_name = "beta-tests" } ] ~actual;
  Ok ()

let test_test_event_to_json_serializes_summary = fun _ctx ->
  match Riot_build.test_event_to_json (Riot_build.Summary { total = 3; passed = 2; failed = 1 }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "TestSummary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal ~expected:(Some (Data.Json.Int 3)) ~actual:(List.assoc_opt "total" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for summary event"

let tests =
  let open Test in [
    case "test runtime: collect test suites" test_collect_test_suites_filters_workspace_binaries;
    case "test runtime: collect test suites by suite name" test_collect_test_suites_filters_by_suite_name;
    case "test runtime: summary event json" test_test_event_to_json_serializes_summary;
  ]

let name = "Riot Build Test Runtime Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
