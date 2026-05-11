open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let write_text = fun path content ->
  let* () =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:IO.error_message
  in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let write_vendor_package = fun workspace_root name ->
  let package_root = Path.(workspace_root / Path.v "vendor" / Path.v name) in
  let module_name = String.capitalize_ascii name in
  let manifest =
    {|[package]
name = "|}
    ^ name
    ^ {|"
version = "0.1.0"

[lib]
path = "src/|}
    ^ module_name
    ^ {|.ml"
|}
  in
  let source = "let label = \"" ^ name ^ "\"\n" in
  let* () = write_text Path.(package_root / Path.v "riot.toml") manifest in
  write_text Path.(package_root / Path.v "src" / Path.v (module_name ^ ".ml")) source

let test_package_commands_handle_multiple_local_dependencies =
  Test.case
    ~size:Test.Large
    "riot add/update/rm handle multiple local path dependencies"
    (fun ctx ->
      let workspace_name = "package-cmd-e2e" in
      with_initialized_workspace
        ctx
        workspace_name
        (fun workspace_root ->
          let starter_manifest =
            Path.(workspace_root / Path.v "packages" / Path.v workspace_name / Path.v "riot.toml")
          in
          let lockfile = Path.(workspace_root / Path.v "riot.lock") in
          let* () = write_vendor_package workspace_root "widgets" in
          let* () = write_vendor_package workspace_root "gadgets" in
          let* add_output =
            run_riot
              ctx
              ~cwd:workspace_root
              [
                "add";
                "-p";
                workspace_name;
                "--json";
                "../../vendor/widgets";
                "../../vendor/gadgets";
              ]
          in
          let* add_output = expect_success ~cmd:"riot add -p package-cmd-e2e" add_output in
          let* () =
            assert_output_contains
              ~cmd:"riot add -p package-cmd-e2e"
              add_output
              "riot.deps.manifest.updated"
          in
          let* () = assert_contains starter_manifest {|widgets = { path = "../../vendor/widgets" }|} in
          let* () = assert_contains starter_manifest {|gadgets = { path = "../../vendor/gadgets" }|} in
          let* () = assert_contains lockfile {|name = "widgets"|} in
          let* () = assert_contains lockfile {|name = "gadgets"|} in
          let* update_output =
            run_riot ctx ~cwd:workspace_root [ "update"; "--json"; "widgets"; "gadgets"; ]
          in
          let* _ = expect_success ~cmd:"riot update widgets gadgets" update_output in
          let* remove_output =
            run_riot
              ctx
              ~cwd:workspace_root
              [ "rm"; "-p"; workspace_name; "--json"; "widgets"; "gadgets"; ]
          in
          let* remove_output = expect_success ~cmd:"riot rm -p package-cmd-e2e" remove_output in
          let* () =
            assert_output_contains
              ~cmd:"riot rm -p package-cmd-e2e"
              remove_output
              "riot.deps.manifest.updated"
          in
          let* () = assert_not_contains starter_manifest "widgets" in
          let* () = assert_not_contains starter_manifest "gadgets" in
          let* () = assert_not_contains lockfile {|name = "widgets"|} in
          assert_not_contains lockfile {|name = "gadgets"|}))

let tests = [ test_package_commands_handle_multiple_local_dependencies ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:package-commands" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
