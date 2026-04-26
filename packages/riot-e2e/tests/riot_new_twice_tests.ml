open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let test_riot_new_twice_keeps_both_packages_usable =
  Test.case
    ~size:Test.Large
    "riot new twice in one workspace keeps both packages usable"
    (fun ctx ->
      let workspace_name = "riot-new-twice" in
      let library_package_name = "extra-library" in
      let binary_package_name = "extra-binary" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let library_root = Path.(workspace_root / Path.v "packages" / Path.v library_package_name) in
          let binary_root = Path.(workspace_root / Path.v "packages" / Path.v binary_package_name) in
          let* new_library_output =
            run_riot ctx ~cwd:workspace_root [ "new"; "--lib"; "./packages/extra-library" ] in
          let* _ = expect_success ~cmd:"riot new --lib" new_library_output in
          let* new_binary_output =
            run_riot ctx ~cwd:workspace_root [ "new"; "--bin"; "./packages/extra-binary" ] in
          let* _ = expect_success ~cmd:"riot new --bin" new_binary_output in
          let* () = assert_exists Path.(library_root / Path.v "riot.toml") in
          let* () = assert_exists Path.(binary_root / Path.v "riot.toml") in
          let* () = assert_exists Path.(binary_root / Path.v "src" / Path.v "main.ml") in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "riot.toml")
              {|  "packages/extra-library",|} in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "riot.toml")
              {|  "packages/extra-binary",|} in
          let* build_library_output =
            run_riot ctx ~cwd:workspace_root [ "build"; "-p"; library_package_name ] in
          let* _ = expect_success ~cmd:"riot build -p extra-library" build_library_output in
          let* build_binary_output =
            run_riot ctx ~cwd:workspace_root [ "build"; "-p"; binary_package_name ] in
          let* _ = expect_success ~cmd:"riot build -p extra-binary" build_binary_output in
          let* run_output = run_riot ctx ~cwd:workspace_root [ "run"; binary_package_name ] in
          let* _ = expect_success ~cmd:"riot run extra-binary" run_output in
          if String.contains run_output.stdout "Hello, World!" then
            Ok ()
          else
            Error ("expected riot run extra-binary to print Hello, World!, got: "
            ^ render_output run_output)))

let tests = [ test_riot_new_twice_keeps_both_packages_usable ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-new-twice" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
