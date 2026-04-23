open Std
open Std.Result.Syntax
open Riot_e2e
module Test = Std.Test

let test_riot_init_default_workspace_builds_and_tests =
  Test.case ~size:Test.Large "riot init default workspace builds and tests"
    (fun ctx ->
      let workspace_name = "hello-e2e" in
      with_initialized_workspace ctx workspace_name
        (fun workspace_root ->
          let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
          let* () = assert_exists
            Path.(workspace_root / Path.v "packages" / Path.v workspace_name / Path.v "riot.toml") in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build" ] in
          let* _ = expect_success ~cmd:"riot build" build_output in
          let* test_output = run_riot ctx ~cwd:workspace_root [ "test" ] in
          let* _ = expect_success ~cmd:"riot test" test_output in
          Ok ()))

let test_riot_init_default_workspace_has_no_runnable_binary =
  Test.case ~size:Test.Large "riot init default workspace reports no runnable binaries"
    (fun ctx ->
      let workspace_name = "hello-e2e" in
      with_initialized_workspace ctx workspace_name
        (fun workspace_root ->
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run" ] in
          let* _ = expect_failure_contains ~cmd:"riot run" ~needle:"no runnable binaries found" run_output in
          Ok ()))

let test_riot_init_binary_workspace_runs =
  Test.case ~size:Test.Large "riot init --bin workspace runs generated starter binary"
    (fun ctx ->
      let workspace_name = "hello-world" in
      with_initialized_workspace ~init_args:[ "--bin" ] ctx workspace_name
        (fun workspace_root ->
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run" ] in
          let* _ = expect_success ~cmd:"riot run" run_output in
          if String.contains run_output.stdout "Hello from hello-world" then
            Ok ()
          else
            Error ("expected generated starter binary greeting, got: " ^ render_output run_output)))

let test_riot_init_dotted_workspace_normalizes_starter_package =
  Test.case ~size:Test.Large "riot init dotted workspace builds with normalized starter package"
    (fun ctx ->
      let workspace_name = "arewedown.dev" in
      let starter_package_name = "arewedown-dev" in
      with_initialized_workspace ctx workspace_name
        (fun workspace_root ->
          let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "arewedown.dev"|} in
          let starter_manifest =
            Path.(workspace_root / Path.v "packages" / Path.v starter_package_name / Path.v "riot.toml") in
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
  test_riot_init_dotted_workspace_normalizes_starter_package;
]

let () =
  Actors.run
    ~main:(fun ~args ->
      Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-init" ~tests ~args ())
    ~args:Env.args
    ()
