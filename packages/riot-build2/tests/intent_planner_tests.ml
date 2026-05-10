open Std

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let package_manifest = fun name ->
  let name = package name in
  Riot_model.Package.synthetic
    ~name
    ~path:Path.(Path.v "." / Path.v (Riot_model.Package_name.to_string name))
    ~relative_path:(Path.v (Riot_model.Package_name.to_string name))
  |> Riot_model.Package_manifest.from_package

let executor_workspace =
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-intent-tests")
    ~packages:[ package_manifest "std" ]
    ()

let executor_config = fun ?parallelism ?on_event () ->
  Config.make
    ~workspace:executor_workspace
    ?parallelism
    ?on_event
    ()

let unexpected_node = fun node ->
  Error (Error.ExecutorInvariantViolated {
    message = "unexpected node in intent planner test: "
    ^ Work_node.Node_id.to_string (Work_node.id node);
  })

let expect_actions = fun ~expected actual ->
  if actual = expected then
    Ok ()
  else
    Error "unexpected goals"

let run_intent_actions = fun intent ->
  let summary = Work_graph.run_intent ~config:(executor_config ~parallelism:1 ()) intent in
  if Executor.has_failures summary then
    Error "intent graph failed"
  else
    summary
    |> Work_graph.completed_goals
    |> Result.ok

let test_executor_drains_spawned_nodes = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  let child = Goal.RunBinary {
    binary = Goal.BinaryInPackage (package "std", "server");
    args = [];
    profile = Riot_model.Profile.debug;
    target = linux;
  }
  in
  let execute = fun _context node ->
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let second = Goal.RunBinary {
          binary = Goal.BinaryInPackage (package "std", "server");
          args = [ "--debug" ];
          profile = Riot_model.Profile.debug;
          target = linux;
        }
        in
        Ok (Work_result.Complete [
          Work_request.existing (Work_node.GoalKey child);
          Work_request.existing (Work_node.GoalKey second);
        ])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let seed =
    Work_node.user_intent
      ~id:(Work_node.Node_id.from_int 1)
      (User_intent.run ~runnable:(User_intent.ByName "server") ~target:linux ())
  in
  let summary =
    Executor.Runner.run_with_handlers
      ~config:(executor_config ~parallelism:2 ())
      ~execution_mode:(fun _node -> Work_node.Concrete)
      ~seeds:[ seed ]
      ~execute
      ()
  in
  if Int.equal summary.completed_count 3 && Int.equal summary.failed_count 0 then
    Ok ()
  else
    Error ("expected three completed nodes and no failures, got completed="
    ^ Int.to_string summary.completed_count
    ^ " failed="
    ^ Int.to_string summary.failed_count)

let test_build_intent_expands_named_packages = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.build
    ~packages:(User_intent.NamedPackages [ package "std"; package "riot-cli" ])
    ~targets:(User_intent.ManyTargets [ linux ])
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.BuildPackage {
            package = package "std";
            scope = Goal.Runtime;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
          Goal.BuildPackage {
            package = package "riot-cli";
            scope = Goal.Runtime;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_build_intent_defaults_to_workspace_members = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.build ~targets:(User_intent.ManyTargets [ linux ]) ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.BuildPackage {
            package = package "std";
            scope = Goal.Runtime;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_build_intent_expands_profiles = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.build
    ~packages:(User_intent.NamedPackages [ package "std" ])
    ~profiles:(User_intent.ManyProfiles [ Riot_model.Profile.debug; Riot_model.Profile.release ])
    ~targets:(User_intent.ManyTargets [ linux ])
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.BuildPackage {
            package = package "std";
            scope = Goal.Runtime;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
          Goal.BuildPackage {
            package = package "std";
            scope = Goal.Runtime;
            profile = Riot_model.Profile.release;
            target = linux;
          };
        ]
        actual)

let test_test_intent_preserves_filter = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.test
    ~packages:(User_intent.NamedPackages [ package "std" ])
    ~filter:"parser"
    ~targets:(User_intent.ManyTargets [ linux ])
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.RunTests {
            package = package "std";
            filter = Some "parser";
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_test_intent_expands_packages_individually = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.test
    ~packages:(User_intent.NamedPackages [ package "std"; package "kernel" ])
    ~targets:(User_intent.ManyTargets [ linux ])
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.RunTests {
            package = package "std";
            filter = None;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
          Goal.RunTests {
            package = package "kernel";
            filter = None;
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_run_intent_by_name_preserves_binary_and_args = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.run
    ~runnable:(User_intent.ByName "server")
    ~args:[ "--port"; "8080" ]
    ~profile:Riot_model.Profile.debug
    ~target:linux
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.RunBinary {
            binary = Goal.BinaryByName "server";
            args = [ "--port"; "8080" ];
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_run_intent_scoped_package_defaults_binary = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.run
    ~runnable:(User_intent.Scoped { package = package "std"; binary = None })
    ~profile:Riot_model.Profile.debug
    ~target:linux
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.RunBinary {
            binary = Goal.DefaultBinaryInPackage (package "std");
            args = [];
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let test_run_intent_preserves_binary_and_args = fun _ctx ->
  let linux = target "x86_64-unknown-linux-gnu" in
  User_intent.run
    ~runnable:(User_intent.Scoped { package = package "std"; binary = Some "server" })
    ~args:[ "--port"; "8080" ]
    ~profile:Riot_model.Profile.debug
    ~target:linux
    ()
  |> run_intent_actions
  |> Result.and_then
    ~fn:(fun actual ->
      expect_actions
        ~expected:[
          Goal.RunBinary {
            binary = Goal.BinaryInPackage (package "std", "server");
            args = [ "--port"; "8080" ];
            profile = Riot_model.Profile.debug;
            target = linux;
          };
        ]
        actual)

let tests =
  Test.[
    case "graph executor drains nodes spawned by a seed" test_executor_drains_spawned_nodes;
    case "build intent expands named packages" test_build_intent_expands_named_packages;
    case
      "build intent defaults to workspace members"
      test_build_intent_defaults_to_workspace_members;
    case "build intent expands profiles" test_build_intent_expands_profiles;
    case "test intent preserves filter" test_test_intent_preserves_filter;
    case "test intent expands packages individually" test_test_intent_expands_packages_individually;
    case
      "run intent by name preserves binary and args"
      test_run_intent_by_name_preserves_binary_and_args;
    case "run intent scoped package defaults binary" test_run_intent_scoped_package_defaults_binary;
    case "run intent preserves binary and args" test_run_intent_preserves_binary_and_args;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_intent_planner_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
