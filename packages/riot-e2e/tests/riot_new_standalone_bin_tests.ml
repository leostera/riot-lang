open Std
open Std.Result.Syntax
open Riot_e2e
module Test = Std.Test

let test_riot_new_binary_creates_a_standalone_package_that_runs =
  Test.case ~size:Test.Large "riot new --bin creates a standalone package that runs"
    (fun ctx ->
      let package_name = "standalone-binary" in
      with_tempdir_result ~prefix:"riot_e2e_new_standalone_bin_"
        (fun root ->
          let package_root = Path.(root / Path.v package_name) in
          let* new_output = run_riot ctx ~cwd:root [ "new"; "--bin"; package_name ] in
          let* _ = expect_success ~cmd:"riot new --bin" new_output in
          let* () = assert_exists Path.(package_root / Path.v "riot.toml") in
          let* () = assert_exists Path.(package_root / Path.v "src" / Path.v "main.ml") in
          let* () = assert_contains Path.(package_root / Path.v "riot.toml") {|[package]|} in
          let* build_output = run_riot ctx ~cwd:package_root [ "build" ] in
          let* _ = expect_success ~cmd:"riot build" build_output in
          let* run_output = run_riot ctx ~cwd:package_root [ "run" ] in
          let* _ = expect_success ~cmd:"riot run" run_output in
          if String.contains run_output.stdout "Hello, World!" then
            Ok ()
          else
            Error ("expected riot run to print Hello, World!, got: " ^ render_output run_output)))

let tests = [ test_riot_new_binary_creates_a_standalone_package_that_runs ]

let main ~args = Test.Cli.main
  ~execution_mode:Test.Cli.Linear
  ~name:"riot-e2e:riot-new-standalone-bin"
  ~tests
  ~args
  ()

let () = Runtime.run ~main ~args:Env.args ()
