open Std
module Test = Std.Test

let ( let* ) value fn = Result.and_then value ~fn

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

let parse_new = fun args ->
  match ArgParser.get_matches Riot_cli.New.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid utf-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system returned invalid utf-8 for "
  ^ syscall
  ^ ": "
  ^ path
  | Path.SystemError message -> message

let with_current_dir_result = fun dir fn ->
  match Env.current_dir () with
  | Error err -> Error (path_error_message err)
  | Ok original ->
      match Env.set_current_dir dir with
      | Error err -> Error (path_error_message err)
      | Ok () ->
          let restore () =
            match Env.set_current_dir original with
            | Ok () -> ()
            | Error _ -> ()
          in
          (
            try
              let result = fn () in
              let () = restore () in
              result
            with
            | exn ->
                let () = restore () in
                raise exn
          )

let with_current_dir_exn_result = fun dir fn ->
  match Env.current_dir () with
  | Error err -> Error (Failure (path_error_message err))
  | Ok original ->
      match Env.set_current_dir dir with
      | Error err -> Error (Failure (path_error_message err))
      | Ok () ->
          let restore () =
            match Env.set_current_dir original with
            | Ok () -> ()
            | Error _ -> ()
          in
          (
            try
              let result = fn () in
              let () = restore () in
              result
            with
            | exn ->
                let () = restore () in
                raise exn
          )

let test_add_bootstraps_empty_workspace_outside_workspace = fun _ctx ->
  with_tempdir_result "riot_cli_add_bootstrap"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "workspace") in
      let manifest_path = Path.(workspace_root / Path.v "riot.toml") in
      let lockfile_path = Path.(workspace_root / Path.v "riot.lock") in
      let* () = Result.map_err (Fs.create_dir_all workspace_root) ~fn:IO.error_message in
      let* () = Riot_cli.Add.bootstrap_empty_workspace ~root:workspace_root
      |> Result.map_err ~fn:Riot_cli.Add.message in
      let* manifest_source = Result.map_err (Fs.read manifest_path) ~fn:IO.error_message in
      let* lockfile = Riot_deps.Lockfile_store.read ~workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected lockfile read to succeed: " ^ Riot_deps.Lockfile_store.error_message err) in
      let* matches = parse_add [ "add"; "hello" ] in
      let* selection = Riot_cli.Add.selection_of_matches ~default_selection:Riot_deps.Workspace matches
      |> Result.map_err ~fn:Riot_cli.Add.message in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      let* (_workspace, load_errors) = Riot_model.Workspace_manager.scan workspace_manager workspace_root
      |> Result.map_err
        ~fn:(fun err ->
          "expected workspace scan to succeed: " ^ Riot_model.Workspace_manager.scan_error_message err) in
      if not (String.contains manifest_source "[workspace]") then
        Error "expected bootstrap manifest to include [workspace]"
      else if not (String.contains manifest_source "members = []") then
        Error "expected bootstrap manifest to start with an empty workspace"
      else
        match lockfile with
        | None -> Error "expected add to create riot.lock"
        | Some lockfile ->
            let* manifest_exists = Result.map_err (Fs.exists manifest_path) ~fn:IO.error_message in
            let* lock_exists = Result.map_err (Fs.exists lockfile_path) ~fn:IO.error_message in
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

let test_add_message_renders_typed_command_errors = fun _ctx ->
  let open Riot_cli.Add in
    let manifest_path = Path.v "/workspace/riot.toml" in
    let bootstrap_message = message
      (WorkspaceBootstrapFailed (BootstrapDependencyHashFailed (Riot_deps.Lock_refresh.ManifestMustBeTable {
        manifest_path
      }))) in
    let load_message = message
      (WorkspaceLoadFailed (WorkspaceLoadHadErrors [
        Riot_model.Workspace_manager.PackageTomlParseFailed {
          package = "dep";
          path = "deps/dep/riot.toml"
        }
      ])) in
    let cwd_message = message (CurrentDirUnavailable (Path.SystemError "cwd unavailable")) in
    let expected_bootstrap = "failed to initialize riot workspace: manifest '/workspace/riot.toml' must decode to a TOML table" in
    let expected_load = "failed to load initialized riot workspace: package 'dep': failed to parse riot.toml at path deps/dep/riot.toml" in
    let expected_cwd = "failed to determine current directory: cwd unavailable" in
    if not (String.equal bootstrap_message expected_bootstrap) then
      Error ("unexpected add bootstrap message: " ^ bootstrap_message)
    else if not (String.equal load_message expected_load) then
      Error ("unexpected add workspace load message: " ^ load_message)
    else if not (String.equal cwd_message expected_cwd) then
      Error ("unexpected add current-dir message: " ^ cwd_message)
    else
      Ok ()

let test_add_accepts_multiple_dependencies = fun _ctx ->
  let* matches = parse_add [ "add"; "std"; "serde-json"; "../widgets" ] in
  Test.assert_equal
    ~expected:[ "std"; "serde-json"; "../widgets" ]
    ~actual:(ArgParser.get_many matches "dependency");
  Ok ()

let test_remove_outside_workspace_message = fun _ctx ->
  let* matches = parse_remove [ "rm"; "hello" ] in
  let* () = Result.map_err (Riot_cli.Remove.run_without_workspace matches) ~fn:Exception.to_string in
  Test.assert_equal ~expected:"No riot.toml, so nothing to remove" ~actual:Riot_cli.Remove.no_workspace_message;
  Ok ()

let test_remove_message_renders_typed_command_errors = fun _ctx ->
  let open Riot_cli.Remove in
    let package_message = message (InvalidPackageName Riot_model.Package_name.Empty) in
    let cwd_message = message (CurrentDirUnavailable (Path.SystemError "cwd unavailable")) in
    let expected_package = Riot_model.Package_name.error_message Riot_model.Package_name.Empty in
    let expected_cwd = "failed to determine current directory: cwd unavailable" in
    if not (String.equal package_message expected_package) then
      Error ("unexpected remove package-name message: " ^ package_message)
    else if not (String.equal cwd_message expected_cwd) then
      Error ("unexpected remove current-dir message: " ^ cwd_message)
    else
      Ok ()

let test_remove_accepts_multiple_dependencies = fun _ctx ->
  let* matches = parse_remove [ "rm"; "std"; "serde-json" ] in
  Test.assert_equal
    ~expected:[ "std"; "serde-json" ]
    ~actual:(ArgParser.get_many matches "dependency");
  Ok ()

let test_update_outside_workspace_message = fun _ctx ->
  let* matches = parse_update [ "update" ] in
  let* () = Result.map_err (Riot_cli.Update_cmd.run_without_workspace matches) ~fn:Exception.to_string in
  Test.assert_equal ~expected:"No riot.toml, so nothing to update" ~actual:Riot_cli.Update_cmd.no_workspace_message;
  Ok ()

let test_update_accepts_package_names = fun _ctx ->
  let* matches = parse_update [ "update"; "std"; "serde-json" ] in
  Test.assert_equal ~expected:[ "std"; "serde-json" ] ~actual:(ArgParser.get_many matches "package");
  Ok ()

let test_new_outside_workspace_creates_standalone_package = fun _ctx ->
  with_tempdir_result "riot_cli_new_standalone"
    (fun tempdir ->
      let package_root = Path.(tempdir / Path.v "hello-world") in
      let* matches = parse_new [ "new"; "hello-world" ] in
      let* () = with_current_dir_exn_result tempdir (fun () -> Riot_cli.New.run matches)
      |> Result.map_err ~fn:Exception.to_string in
      let* package_exists = Result.map_err
        (Fs.exists Path.(package_root / Path.v "riot.toml"))
        ~fn:IO.error_message in
      let* main_exists = Result.map_err
        (Fs.exists Path.(package_root / Path.v "src" / Path.v "HelloWorld.ml"))
        ~fn:IO.error_message in
      let* manifest_source = Result.map_err
        (Fs.read Path.(package_root / Path.v "riot.toml"))
        ~fn:IO.error_message in
      if package_exists && main_exists && String.contains manifest_source "[package]" then
        Ok ()
      else
        Error "expected riot new outside a workspace to create a standalone package")

let tests =
  Test.[
    case "package commands: add bootstraps an empty workspace outside a workspace" test_add_bootstraps_empty_workspace_outside_workspace;
    case "package commands: add renders typed command errors" test_add_message_renders_typed_command_errors;
    case "package commands: add accepts multiple dependencies" test_add_accepts_multiple_dependencies;
    case "package commands: remove outside a workspace reports no riot.toml" test_remove_outside_workspace_message;
    case "package commands: remove renders typed command errors" test_remove_message_renders_typed_command_errors;
    case "package commands: remove accepts multiple dependencies" test_remove_accepts_multiple_dependencies;
    case "package commands: update outside a workspace reports no riot.toml" test_update_outside_workspace_message;
    case "package commands: update accepts package names" test_update_accepts_package_names;
    case "package commands: new outside a workspace creates a standalone package" test_new_outside_workspace_creates_standalone_package;
  ]

let name = "Riot CLI Package Command Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
