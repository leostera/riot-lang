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

let test_build_scope_for_test_binary_uses_dev = fun _ctx ->
  let workspace = make_workspace
    [ Riot_model.Package.{ name = "pm_tests"; path = Path.v "tests/pm_tests.ml" } ] in
  Test.assert_equal
    ~expected:Riot_build.Dev
    ~actual:(Riot_build.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"pm_tests");
  Ok ()

let test_run_event_to_json_serializes_running_binary = fun _ctx ->
  match Riot_build.run_event_to_json
    (Riot_build.RunningBinary {
      package = "demo";
      binary = "pm_tests";
      args = [ "run-tests"; "query" ]
    }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "RunningBinary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal
        ~expected:(Some (Data.Json.String "demo"))
        ~actual:(List.assoc_opt "package" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for running binary event"

let test_run_error_message_names_external_target_load_failure = fun _ctx ->
  Test.assert_equal
    ~expected:"failed to load external target 'leostera/riot': boom"
    ~actual:
      (Riot_build.run_error_message
         (Riot_build.ExternalTargetLoadFailed { target = "leostera/riot"; reason = "boom" }));
  Ok ()

let tests =
  let open Test in [
    case "run: test binaries use dev scope" test_build_scope_for_test_binary_uses_dev;
    case "run: running binary event json" test_run_event_to_json_serializes_running_binary;
    case "run: external target load error message" test_run_error_message_names_external_target_load_failure;
  ]

let name = "Riot Build Run Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
