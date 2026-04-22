open Std
open Riot_build
open Riot_model
module Test = Std.Test
module Diagnostic_rewrite = Riot_build.Internal.Diagnostic_rewrite

let package_name = fun value ->
  Package_name.from_string value |> Result.expect ~msg:("expected valid package name: " ^ value)

let make_warning = fun path ->
  String.concat
    "\n"
    [
      "File \"" ^ path ^ "\", line 25, characters 8-26:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             ^^^^^^^^^^^^^^^^^^";
      "Warning 26 [unused-var]: unused variable displayed_packages.";
    ]

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let test_rewrite_ocamlc_result_rewrites_workspace_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"diagnostic_rewrite_test"
      (fun tmpdir ->
        let package = make_package ~root:tmpdir ~name:"riot-cli" in
        let package_src = Path.(package.path / Path.v "src") in
        let sandbox_dir =
          Path.(tmpdir / Path.v "_build" / Path.v "sandbox" / Path.v "riot-cli-a9441944") in
        let _ = Fs.create_dir_all package_src |> Result.expect ~msg:"failed to create package src" in
        let _ = Fs.write
          "let displayed_packages = HashMap.create ()"
          Path.(package_src / Path.v "install.ml")
        |> Result.expect ~msg:"failed to write package source" in
        let diagnostic = make_warning
          (Path.to_string Path.(sandbox_dir / Path.v "src" / Path.v "install.ml"))
        |> Riot_toolchain.Ocamlc.Diagnostic.parse
        |> List.head
        |> Option.expect ~msg:"expected one parsed diagnostic" in
        let result = Riot_toolchain.Ocamlc.Success { message = ""; diagnostics = [ diagnostic ] } in
        let rewritten = Diagnostic_rewrite.rewrite_ocamlc_result ~package ~sandbox_dir result in
        let warnings = Riot_toolchain.Ocamlc.get_ocamlc_warnings rewritten in
        match warnings with
        | [ warning ] when String.contains warning "./packages/riot-cli/src/install.ml" -> Ok ()
        | [ warning ] -> Error ("expected rewritten workspace path, got: " ^ warning)
        | _ -> Error "expected exactly one rewritten warning")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_rewrite_ocamlc_result_leaves_generated_paths_alone = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"diagnostic_rewrite_generated_test"
      (fun tmpdir ->
        let package = make_package ~root:tmpdir ~name:"riot-cli" in
        let sandbox_dir =
          Path.(tmpdir / Path.v "_build" / Path.v "sandbox" / Path.v "riot-cli-a9441944") in
        let diagnostic = make_warning
          (Path.to_string Path.(sandbox_dir / Path.v "Riot_cli__Aliases.ml.gen"))
        |> Riot_toolchain.Ocamlc.Diagnostic.parse
        |> List.head
        |> Option.expect ~msg:"expected one parsed diagnostic" in
        let rewritten = Diagnostic_rewrite.rewrite_ocamlc_result
          ~package
          ~sandbox_dir
          (Riot_toolchain.Ocamlc.Failed {
            message = "Command failed with status 2";
            diagnostics = [ diagnostic ]
          }) in
        let rendered = Riot_toolchain.Ocamlc.get_output rewritten in
        if
          String.contains
            rendered
            (Path.to_string Path.(sandbox_dir / Path.v "Riot_cli__Aliases.ml.gen"))
        then
          Ok ()
        else
          Error ("expected generated sandbox path to stay unchanged, got: " ^ rendered))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let name = "diagnostic_rewrite_tests"

let tests =
  Test.[
    case "rewrite ocamlc result rewrites workspace paths" test_rewrite_ocamlc_result_rewrites_workspace_paths;
    case "rewrite ocamlc result leaves generated paths alone" test_rewrite_ocamlc_result_leaves_generated_paths_alone;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
