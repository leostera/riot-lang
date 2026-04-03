open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let parse_add = fun args ->
  match ArgParser.get_matches Riot_cli.Add.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_remove = fun args ->
  match ArgParser.get_matches Riot_cli.Remove.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_update = fun args ->
  match ArgParser.get_matches Riot_cli.Update_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_add_bootstraps_empty_workspace_outside_workspace = fun _ctx ->
  with_tempdir_result "riot_cli_add_bootstrap"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "workspace") in
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      let lockfile_path = Path.(workspace_root / Path.v "riot.lock") in
      let* () = Fs.create_dir_all workspace_root |> Result.map_error IO.error_message in
      let* () =
        Riot_cli.Add.bootstrap_empty_workspace ~root:workspace_root
        |> Result.map_error Riot_cli.Add.message
      in
      let* manifest_source = Fs.read manifest_path |> Result.map_error IO.error_message in
      let* lockfile = Riot_deps.Lockfile_store.read ~workspace_root
      |> Result.map_error (fun err -> "expected lockfile read to succeed: " ^ err) in
      let* matches = parse_add [ "add"; "hello" ] in
      let* selection =
        Riot_cli.Add.selection_of_matches ~default_selection:Riot_deps.Workspace matches
        |> Result.map_error Riot_cli.Add.message
      in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      let* _workspace, load_errors =
        Riot_model.Workspace_manager.scan workspace_manager workspace_root
        |> Result.map_error (fun err -> "expected workspace scan to succeed: " ^ err)
      in
      if not (String.contains manifest_source "[workspace]") then
        Error "expected bootstrap manifest to include [workspace]"
      else if not (String.contains manifest_source "members = []") then
        Error "expected bootstrap manifest to start with an empty workspace"
      else
        match lockfile with
        | None -> Error "expected add to create riot.lock"
        | Some lockfile ->
            let* manifest_exists = Fs.exists manifest_path |> Result.map_error IO.error_message in
            let* lock_exists = Fs.exists lockfile_path |> Result.map_error IO.error_message in
            if
              manifest_exists
              && lock_exists
              && List.is_empty lockfile.packages
              && load_errors = []
              && selection = Riot_deps.Workspace
            then
              Ok ()
            else
              Error "expected bootstrap to create a loadable empty workspace and default add to the workspace root")

let test_remove_outside_workspace_message = fun _ctx ->
  let* matches = parse_remove [ "rm"; "hello" ] in
  let* () = Riot_cli.Remove.run_without_workspace matches |> Result.map_error Exception.to_string in
  Test.assert_equal ~expected:"No riot.toml, so nothing to remove" ~actual:Riot_cli.Remove.no_workspace_message;
  Ok ()

let test_update_outside_workspace_message = fun _ctx ->
  let* matches = parse_update [ "update" ] in
  let* () = Riot_cli.Update_cmd.run_without_workspace matches |> Result.map_error Exception.to_string in
  Test.assert_equal ~expected:"No riot.toml, so nothing to update" ~actual:Riot_cli.Update_cmd.no_workspace_message;
  Ok ()

let tests =
  Test.[
    case "package commands: add bootstraps an empty workspace outside a workspace" test_add_bootstraps_empty_workspace_outside_workspace;
    case "package commands: remove outside a workspace reports no riot.toml" test_remove_outside_workspace_message;
    case "package commands: update outside a workspace reports no riot.toml" test_update_outside_workspace_message;
  ]

let name = "Riot CLI Package Command Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
