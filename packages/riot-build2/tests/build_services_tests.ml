open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let dependency_source =
  Riot_model.Package.{
    workspace = true;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = None;
  }

let package_manifest = fun ?(dependencies = []) name ->
  let name = package name in
  Riot_model.Package.make
    ~name
    ~path:Path.(Path.v "." / Path.v (Riot_model.Package_name.to_string name))
    ~relative_path:(Path.v (Riot_model.Package_name.to_string name))
    ~dependencies
    ()
  |> Riot_model.Package_manifest.from_package

let workspace =
  let dep_name = package "dep" in
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-build-services-tests")
    ~packages:[
      package_manifest "dep";
      package_manifest
        ~dependencies:[ Riot_model.Package.{ name = dep_name; source = dependency_source } ]
        "app";
    ]
    ()

let config = fun () -> Config.make ~workspace ~parallelism:1 ()

let build_goal = fun name ->
  Goal.BuildPackage {
    package = package name;
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let has_goal_key = fun keys goal ->
  List.any
    keys
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Work_node.GoalKey got -> got = goal
      | _ -> false)

let test_build_package_plans_package_dependencies_before_execution = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let app_goal = build_goal "app" in
  let dep_goal = build_goal "dep" in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) app_goal in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_goal_key keys dep_goal then
        Ok ()
      else
        Error "expected app build goal to plan dep build goal before execution")

let tests =
  Test.[
    case
      "build package plans package dependencies before execution"
      test_build_package_plans_package_dependencies_before_execution;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_build_services_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
