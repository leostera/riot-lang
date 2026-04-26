open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let package_module_name = fun name ->
  String.split ~by:"-" name
  |> List.map ~fn:String.capitalize_ascii
  |> String.concat ""

let test_riot_new_library_creates_a_standalone_package_that_builds =
  Test.case
    ~size:Test.Large
    "riot new --lib creates a standalone package that builds"
    (fun ctx ->
      let package_name = "standalone-library" in
      with_tempdir_result
        ~prefix:"riot_e2e_new_standalone_lib_"
        (fun root ->
          let package_root = Path.(root / Path.v package_name) in
          let module_name = package_module_name package_name in
          let* new_output = run_riot ctx ~cwd:root [ "new"; "--lib"; package_name ] in let* _ =
            expect_success ~cmd:"riot new --lib" new_output in let* () =
            assert_exists Path.(package_root / Path.v "riot.toml") in let* () =
            assert_exists Path.(package_root / Path.v "src" / Path.v (module_name ^ ".ml")) in let* () =
            assert_exists Path.(package_root / Path.v "src" / Path.v (module_name ^ ".mli")) in let* () =
            assert_contains Path.(package_root / Path.v "riot.toml") {|[package]|} in let* build_output =
            run_riot ctx ~cwd:package_root [ "build" ] in let* _ =
            expect_success ~cmd:"riot build" build_output in Ok ()))

let tests = [ test_riot_new_library_creates_a_standalone_package_that_builds ]

let main ~args =
  Test.Cli.main
    ~execution_mode:Test.Cli.Linear
    ~name:"riot-e2e:riot-new-standalone-lib"
    ~tests
    ~args
    ()

let () = Runtime.run ~main ~args:Env.args ()
