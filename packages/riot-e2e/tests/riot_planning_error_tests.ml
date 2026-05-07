open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let test_build_reports_direct_dependency_module_boundary =
  Test.case
    ~size:Test.Large
    "riot build reports direct dependency module boundary errors"
    (fun ctx ->
      let workspace_name = "module-boundary-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let source =
            Path.(workspace_root
            / Path.v "packages"
            / Path.v workspace_name
            / Path.v "src"
            / Path.v "module_boundary_e2e.ml")
          in
          let invalid_source =
            {|open Std

let hello = fun () ->
  let _ = Kernel.Path.v "riot.toml" in
  "Hello from module-boundary-e2e"
|}
          in
          let* () =
            Fs.write invalid_source source
            |> Result.map_err ~fn:IO.error_message
          in
          let* build_output = run_riot ctx ~cwd:workspace_root [ "build"; "-p"; workspace_name ] in
          let* build_output =
            expect_failure_contains
              ~cmd:"riot build -p module-boundary-e2e"
              ~needle:"Kernel is not available to package module-boundary-e2e"
              build_output
          in
          let* () =
            assert_output_contains
              ~cmd:"riot build -p module-boundary-e2e"
              build_output
              "Riot only exposes modules from this package and its direct dependencies"
          in
          let* () =
            assert_output_contains
              ~cmd:"riot build -p module-boundary-e2e"
              build_output
              "available direct modules"
          in
          assert_output_contains
            ~cmd:"riot build -p module-boundary-e2e"
            build_output
            "or depend through one of the exposed modules above if that is the public API you meant"))

let tests = [ test_build_reports_direct_dependency_module_boundary ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:planning-errors" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
