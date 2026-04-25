open Std
open Std.Result.Syntax
open Riot_e2e
module Test = Std.Test

let test_riot_new_binary_adds_workspace_member_and_runs =
  Test.case ~size:Test.Large "riot new --bin adds the package to workspace members and runs"
    (fun ctx ->
      let workspace_name = "riot-new-bin" in
      let package_name = "extra-binary" in
      with_initialized_workspace ctx workspace_name
        (fun workspace_root ->
          let package_root = Path.(workspace_root / Path.v "packages" / Path.v package_name) in
          let* new_output = run_riot
            ctx
            ~cwd:workspace_root
            [ "new"; "--bin"; "./packages/extra-binary" ] in
          let* _ = expect_success ~cmd:"riot new --bin" new_output in
          let* () = assert_exists Path.(package_root / Path.v "riot.toml") in
          let* () = assert_exists Path.(package_root / Path.v "src" / Path.v "main.ml") in
          let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|  "packages/extra-binary",|} in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build"; "-p"; package_name ] in
          let* _ = expect_success ~cmd:"riot build -p extra-binary" build_output in
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run"; package_name ] in
          let* _ = expect_success ~cmd:"riot run extra-binary" run_output in
          if String.contains run_output.stdout "Hello, World!" then
            Ok ()
          else
            Error ("expected riot run extra-binary to print Hello, World!, got: " ^ render_output run_output)))

let tests = [ test_riot_new_binary_adds_workspace_member_and_runs ]

let main ~args = Test.Cli.main
  ~execution_mode:Test.Cli.Linear
  ~name:"riot-e2e:riot-new-bin"
  ~tests
  ~args
  ()

let () = Runtime.run ~main ~args:Env.args ()
