open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let test_riot_init_default_workspace_builds_and_tests =
  Test.case
    ~size:Test.Large
    "riot init default workspace builds and tests"
    (fun ctx ->
      let workspace_name = "hello-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
          let* () =
            assert_exists
              Path.(workspace_root / Path.v "packages" / Path.v workspace_name / Path.v "riot.toml")
          in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build" ] in
          let* _ = expect_success ~cmd:"riot build" build_output in
          let* test_output = run_riot ctx ~cwd:workspace_root [ "test" ] in
          let* _ = expect_success ~cmd:"riot test" test_output in
          Ok ()))

let test_riot_init_default_workspace_has_no_runnable_binary =
  Test.case
    ~size:Test.Large
    "riot init default workspace reports no runnable binaries"
    (fun ctx ->
      let workspace_name = "hello-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run" ] in
          let* _ =
            expect_failure_contains ~cmd:"riot run" ~needle:"no runnable binaries found" run_output
          in
          Ok ()))

let test_riot_init_binary_workspace_runs =
  Test.case
    ~size:Test.Large
    "riot init --bin workspace runs generated starter binary"
    (fun ctx ->
      let workspace_name = "hello-world" in
      with_initialized_workspace
        ~init_args:[ "--bin" ]
        ctx
        workspace_name
        (fun workspace_root ->
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run" ] in
          let* _ = expect_success ~cmd:"riot run" run_output in
          if String.contains run_output.stdout "Hello from hello-world" then
            Ok ()
          else
            Error ("expected generated starter binary greeting, got: " ^ render_output run_output)))

let test_riot_init_scaffolds_operational_defaults =
  Test.case
    ~size:Test.Large
    "riot init scaffolds operational defaults"
    (fun ctx ->
      let workspace_name = "operational-defaults" in
      with_initialized_workspace
        ~init_args:[ "--bin" ]
        ctx
        workspace_name
        (fun workspace_root ->
          let skill_root =
            Path.(workspace_root / Path.v ".agents" / Path.v "skills" / Path.v "riot")
          in
          let package_root = Path.(workspace_root / Path.v "packages" / Path.v workspace_name) in
          let pre_commit = Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit") in
          let* () = assert_exists Path.(skill_root / Path.v "SKILL.md") in
          let* () = assert_exists Path.(skill_root / Path.v "agents" / Path.v "openai.yaml") in
          let* () = assert_exists Path.(skill_root / Path.v "references" / Path.v "commands.md") in
          let* () = assert_exists Path.(skill_root / Path.v "references" / Path.v "testing.md") in
          let* () = assert_exists Path.(skill_root / Path.v "references" / Path.v "benchmarking.md") in
          let* () = assert_exists Path.(skill_root / Path.v "references" / Path.v "modules.md") in
          let* () = assert_exists Path.(workspace_root / Path.v "config" / Path.v "dev.toml") in
          let* () = assert_exists Path.(workspace_root / Path.v ".riot" / Path.v "config.toml") in
          let* () = assert_exists pre_commit in
          let* () = assert_executable pre_commit in
          let* () = assert_contains pre_commit "riot fmt" in
          let* () = assert_contains pre_commit "riot fix" in
          let* () = assert_contains pre_commit "riot build" in
          let* () = assert_contains pre_commit "riot test --small" in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "config" / Path.v "dev.toml")
              {|name = "operational-defaults"|}
          in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "config" / Path.v "dev.toml")
              "[[log.handler]]"
          in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "config" / Path.v "dev.toml")
              {|type = "stdout"|}
          in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v ".riot" / Path.v "config.toml")
              "[riot.cache]"
          in
          let* () = assert_contains Path.(skill_root / Path.v "SKILL.md") "references/modules.md" in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "commands.md")
              "riot test -p my-package -f parser"
          in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "commands.md")
              "riot bench --iterations 100 --warmup 10"
          in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "testing.md")
              "Writing e2e tests"
          in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "benchmarking.md")
              "riot bench -p my-package -f lookup --compare 5"
          in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "modules.md")
              "Riot does not expose transitive dependencies"
          in
          let* () =
            assert_contains
              Path.(skill_root / Path.v "references" / Path.v "modules.md")
              "Tests inherit normal package dependencies"
          in
          let* () =
            assert_contains
              Path.(package_root / Path.v "src" / Path.v "main.ml")
              "Std.Config.load ();"
          in
          let* () =
            assert_contains
              Path.(package_root / Path.v "src" / Path.v "main.ml")
              "Std.Log.start_link ()"
          in
          let* () = assert_contains Path.(package_root / Path.v "src" / Path.v "main.ml") "println" in
          Ok ()))

let test_riot_run_list_json_reports_generated_binary =
  Test.case
    ~size:Test.Large
    "riot run --list --json reports generated binary"
    (fun ctx ->
      let workspace_name = "run-list-e2e" in
      with_initialized_workspace
        ~init_args:[ "--bin" ]
        ctx
        workspace_name
        (fun workspace_root ->
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run"; "--list"; "--json" ] in
          let* run_output = expect_success ~cmd:"riot run --list --json" run_output in
          let* () =
            assert_output_contains ~cmd:"riot run --list --json" run_output {|"type":"RunList"|}
          in
          let* () =
            assert_output_contains
              ~cmd:"riot run --list --json"
              run_output
              {|"selector":"run-list-e2e:run-list-e2e"|}
          in
          assert_output_contains
            ~cmd:"riot run --list --json"
            run_output
            {|"path":"packages/run-list-e2e/src/main.ml"|}))

let test_riot_test_list_json_filters_generated_starter_test =
  Test.case
    ~size:Test.Large
    "riot test --list --json filters generated starter test"
    (fun ctx ->
      let workspace_name = "test-list-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let* test_output =
            run_riot
              ctx
              ~cwd:workspace_root
              [ "test"; "--list"; "--json"; "-p"; workspace_name; "-f"; "starter"; ]
          in
          let* test_output = expect_success ~cmd:"riot test --list --json" test_output in
          let* () =
            assert_output_contains
              ~cmd:"riot test --list --json"
              test_output
              {|"type":"TestCaseListed"|}
          in
          let* () =
            assert_output_contains
              ~cmd:"riot test --list --json"
              test_output
              {|"name":"starter greeting"|}
          in
          let* () =
            assert_output_contains
              ~cmd:"riot test --list --json"
              test_output
              {|"type":"TestListCompleted"|}
          in
          assert_output_contains ~cmd:"riot test --list --json" test_output {|"test_count":1|}))

let test_riot_bench_list_json_succeeds_without_benchmarks =
  Test.case
    ~size:Test.Large
    "riot bench --list --json succeeds without benchmarks"
    (fun ctx ->
      let workspace_name = "bench-list-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let* bench_output =
            run_riot ctx ~cwd:workspace_root [ "bench"; "--list"; "--json"; "-p"; workspace_name; ]
          in
          let* bench_output = expect_success ~cmd:"riot bench --list --json" bench_output in
          let* () =
            assert_output_contains
              ~cmd:"riot bench --list --json"
              bench_output
              {|"type":"BenchListCompleted"|}
          in
          assert_output_contains
            ~cmd:"riot bench --list --json"
            bench_output
            {|"benchmark_count":0|}))

let test_riot_init_dotted_workspace_normalizes_starter_package =
  Test.case
    ~size:Test.Large
    "riot init dotted workspace builds with normalized starter package"
    (fun ctx ->
      let workspace_name = "arewedown.dev" in
      let starter_package_name = "arewedown-dev" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let* () =
            assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "arewedown.dev"|}
          in
          let starter_manifest =
            Path.(workspace_root
            / Path.v "packages"
            / Path.v starter_package_name
            / Path.v "riot.toml")
          in
          let* () = assert_exists starter_manifest in
          let* () = assert_contains starter_manifest {|name = "arewedown-dev"|} in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build" ] in
          let* _ = expect_success ~cmd:"riot build" build_output in
          let* test_output = run_riot ctx ~cwd:workspace_root [ "test" ] in
          let* _ = expect_success ~cmd:"riot test" test_output in
          Ok ()))

let tests = [
  test_riot_init_default_workspace_builds_and_tests;
  test_riot_init_default_workspace_has_no_runnable_binary;
  test_riot_init_binary_workspace_runs;
  test_riot_init_scaffolds_operational_defaults;
  test_riot_run_list_json_reports_generated_binary;
  test_riot_test_list_json_filters_generated_starter_test;
  test_riot_bench_list_json_succeeds_without_benchmarks;
  test_riot_init_dotted_workspace_normalizes_starter_package;
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-init" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
