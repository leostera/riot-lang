open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let package_module_name = fun name ->
  String.split ~by:"-" name
  |> List.map ~fn:String.capitalize_ascii
  |> String.concat ""

let test_riot_new_library_adds_workspace_member_and_builds =
  Test.case
    ~size:Test.Large
    "riot new --lib adds the package to workspace members and builds"
    (fun ctx ->
      let workspace_name = "riot-new-lib" in
      let package_name = "extra-library" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let package_root = Path.(workspace_root / Path.v "packages" / Path.v package_name) in
          let module_name = package_module_name package_name in
          let* new_output =
            run_riot ctx ~cwd:workspace_root [ "new"; "--lib"; "./packages/extra-library" ]
          in
          let* _ = expect_success ~cmd:"riot new --lib" new_output in
          let* () = assert_exists Path.(package_root / Path.v "riot.toml") in
          let* () = assert_exists Path.(package_root / Path.v "src" / Path.v (module_name ^ ".ml")) in
          let* () = assert_exists Path.(package_root / Path.v "src" / Path.v (module_name ^ ".mli")) in
          let* () =
            assert_contains
              Path.(workspace_root / Path.v "riot.toml")
              {|  "packages/extra-library",|}
          in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build"; "-p"; package_name ] in
          let* _ = expect_success ~cmd:"riot build -p extra-library" build_output in
          Ok ()))

let tests = [ test_riot_new_library_adds_workspace_member_and_builds ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-new-lib" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
