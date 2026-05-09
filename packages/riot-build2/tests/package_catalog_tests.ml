open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let write_file = fun path content ->
  let* () =
    match Path.parent path with
    | Some parent -> Fs.create_dir_all parent
    | None -> Ok ()
  in
  Fs.write content path

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let app_package = package "app"

let with_app_workspace = fun fn ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_package_catalog"
    (fun tempdir ->
      let* () =
        write_file Path.(tempdir / Path.v "riot.toml") "[workspace]\nmembers = [\"app\"]\n"
        |> Result.map_err ~fn:IO.error_message
      in
      let app_dir = Path.(tempdir / Path.v "app") in
      let* () =
        write_file
          Path.(app_dir / Path.v "riot.toml")
          "[package]\nname = \"app\"\nversion = \"0.0.0\"\n"
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        write_file Path.(app_dir / Path.v "src" / Path.v "main.ml") "let () = ()\n"
        |> Result.map_err ~fn:IO.error_message
      in
      match Workspace_loader.load_local ~root:tempdir with
      | Error error -> Error (Workspace_loader.error_message error)
      | Ok workspace -> fn workspace) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_catalog_indexes_manifests_without_realizing = fun _ctx ->
  with_app_workspace
    (fun workspace ->
      let catalog = Package_catalog.create workspace in
      match Package_catalog.find_manifest catalog app_package with
      | None -> Error "catalog should find app manifest"
      | Some manifest ->
          if List.is_empty manifest.declared_binaries then
            Ok ()
          else
            Error "catalog creation should not realize runtime binaries")

let test_catalog_realize_uses_explicit_intent = fun _ctx ->
  with_app_workspace
    (fun workspace ->
      let catalog = Package_catalog.create workspace in
      match Package_catalog.realize catalog ~intent:Riot_model.Package.Runtime app_package with
      | Error error -> Error (Error.message error)
      | Ok package ->
          if
            List.any
              package.Riot_model.Package.binaries
              ~fn:(fun (binary: Riot_model.Package.binary) ->
                String.equal binary.name "app"
                && Path.equal binary.path Path.(Path.v "src" / Path.v "main.ml"))
          then
            Ok ()
          else
            Error "runtime realization should autodiscover src/main.ml binary")

let tests =
  Test.[
    case
      "catalog indexes manifests without realizing"
      test_catalog_indexes_manifests_without_realizing;
    case "catalog realize uses explicit intent" test_catalog_realize_uses_explicit_intent;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_package_catalog_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
