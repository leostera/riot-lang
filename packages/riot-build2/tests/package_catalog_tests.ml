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

let runtime_source_count = fun catalog ->
  Package_catalog.realize catalog ~intent:Riot_model.Package.Runtime app_package
  |> Result.map ~fn:(fun package ->
    List.length package.Riot_model.Package.sources.src)

let expect_source_count = fun label expected actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error (
      label
      ^ " expected "
      ^ Int.to_string expected
      ^ " runtime source(s), got "
      ^ Int.to_string actual
    )

let test_catalog_realization_cache_is_scoped_to_execution = fun _ctx ->
  with_app_workspace
    (fun workspace ->
      let catalog = Package_catalog.create workspace in
      let* first_count = runtime_source_count catalog |> Result.map_err ~fn:Error.message in
      let* () = expect_source_count "first realization" 1 first_count in
      let app_dir =
        match Package_catalog.find_manifest catalog app_package with
        | Some (manifest: Riot_model.Package_manifest.t) -> manifest.path
        | None -> Path.v "."
      in
      let* () =
        write_file
          Path.(app_dir / Path.v "src" / Path.v "extra.ml")
          "let extra = ()\n"
        |> Result.map_err ~fn:IO.error_message
      in
      let* same_execution_count =
        runtime_source_count catalog
        |> Result.map_err ~fn:Error.message
      in
      let* () =
        expect_source_count
          "same execution cached realization"
          1
          same_execution_count
      in
      Package_catalog.begin_execution catalog;
      let* next_execution_count =
        runtime_source_count catalog
        |> Result.map_err ~fn:Error.message
      in
      expect_source_count "next execution fresh realization" 2 next_execution_count)

let tests =
  Test.[
    case
      "catalog indexes manifests without realizing"
      test_catalog_indexes_manifests_without_realizing;
    case "catalog realize uses explicit intent" test_catalog_realize_uses_explicit_intent;
    case
      "catalog realization cache is scoped to execution"
      test_catalog_realization_cache_is_scoped_to_execution;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_package_catalog_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
