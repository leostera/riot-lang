open Std
open Std.Result.Syntax
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let sample_suite = {
  Riot_cli.Test_runtime.package_name = package_name "demo";
  suite_name = "demo_tests";
}

let expect_field = fun fields name expected ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, actual) when String.equal (Data.Json.to_string actual) (Data.Json.to_string expected) -> Ok ()
  | Some (_, actual) ->
      Error
        ("expected field "
        ^ name
        ^ " to equal "
        ^ Data.Json.to_string expected
        ^ ", got "
        ^ Data.Json.to_string actual)
  | None ->
      Error ("missing field " ^ name)

let test_suite_heartbeat_event_to_json = fun _ctx ->
  match
    Riot_cli.Test_runtime.test_event_to_json
      (SuiteHeartbeat {
        suite = sample_suite;
        binary_path = Path.v "/tmp/demo_tests";
        elapsed_us = 1_234;
      })
  with
  | Some (Data.Json.Object fields) ->
      let* () = expect_field fields "type" (Data.Json.String "SuiteHeartbeat") in
      let* () = expect_field fields "package" (Data.Json.String "demo") in
      let* () = expect_field fields "suite" (Data.Json.String "demo_tests") in
      let* () = expect_field fields "binary_path" (Data.Json.String "/tmp/demo_tests") in
      expect_field fields "elapsed_us" (Data.Json.Int 1_234)
  | Some json ->
      Error ("expected object json, got: " ^ Data.Json.to_string json)
  | None ->
      Error "expected suite heartbeat event to render json"

let tests = [
  Test.case "suite heartbeat event renders json" test_suite_heartbeat_event_to_json;
]

let main = fun ~args -> Test.Cli.main ~name:"test_runtime_tests" ~tests ~args

let () = Runtime.run ~main ~args:Env.args ()
