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

let test_load_local_preserves_manifest_shape = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_workspace_loader"
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
      | Ok workspace ->
          match workspace.Riot_model.Workspace.packages with
          | [ (package: Riot_model.Package_manifest.t) ] ->
              if List.is_empty package.declared_binaries then
                Ok ()
              else
                Error "workspace loader realized package binaries while loading manifests"
          | _ -> Error "workspace loader should load exactly one package manifest") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let tests =
  Test.[
    case
      "load local preserves workspace manager manifest shape"
      test_load_local_preserves_manifest_shape;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_workspace_loader_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
