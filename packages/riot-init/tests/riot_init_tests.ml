open Std

module Test = Std.Test

let ( let* ) result fn = Result.and_then result ~fn

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let parse_init = fun args ->
  match ArgParser.get_matches Riot_init.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let rec run_init = fun args ->
  let* _events = run_init_with_events args in
  Ok ()

and run_init_with_events = fun args ->
  let* matches = parse_init args in
  let events = ref [] in
  let* () =
    Riot_init.run ~on_event:(fun event -> events := event :: !events) matches
    |> Result.map_err ~fn:Exception.to_string
  in
  Ok (List.reverse !events)

let assert_exists = fun path ->
  match Fs.exists path with
  | Ok true -> Ok ()
  | Ok false -> Error ("expected file to exist: " ^ Path.to_string path)
  | Error err -> Error (IO.error_message err)

let assert_contains = fun path needle ->
  let* source =
    Fs.read path
    |> Result.map_err ~fn:IO.error_message
  in
  if String.contains source needle then
    Ok ()
  else
    Error ("expected " ^ Path.to_string path ^ " to contain: " ^ needle)

let assert_executable = fun path ->
  let* metadata =
    Fs.metadata path
    |> Result.map_err ~fn:IO.error_message
  in
  if Fs.Permissions.user_execute (Fs.Metadata.permissions metadata) then
    Ok ()
  else
    Error ("expected file to be executable: " ^ Path.to_string path)

let path_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> syscall ^ ": invalid UTF-8 path: " ^ path
  | Path.SystemError message -> message

let package_module_name = fun name ->
  String.split ~by:"-" name
  |> List.map ~fn:String.capitalize_ascii
  |> String.concat ""

let module_file_stem = fun module_name -> String.lowercase_ascii module_name

let with_current_dir_result = fun dir fn ->
  let original =
    Env.current_dir ()
    |> Result.expect ~msg:"expected current directory"
  in
  let result =
    match Env.set_current_dir dir with
    | Ok () -> fn ()
    | Error err -> Error (path_error_to_string err)
  in
  match Env.set_current_dir original with
  | Error err -> Error (path_error_to_string err)
  | Ok () -> result

let completion_event = fun events ->
  events
  |> List.reverse
  |> List.find
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_init.WorkspaceInitializationCompleted _ -> true
      | _ -> false)

let test_init_scaffolds_library_workspace = fun _ctx ->
  with_tempdir_result
    "riot_init_lib"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "demo-app") in
      let* () = run_init [ "init"; Path.to_string workspace_root ] in
      let module_name =
        Riot_model.Module_name.(from_string "demo-app"
        |> to_string)
      in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "Dockerfile") in
      let* () =
        assert_exists
          Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml")
      in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v ".agents"
          / Path.v "skills"
          / Path.v "riot"
          / Path.v "SKILL.md")
      in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v ".agents"
          / Path.v "skills"
          / Path.v "riot"
          / Path.v "references"
          / Path.v "commands.md")
      in
      let* () = assert_exists Path.(workspace_root / Path.v "config" / Path.v "dev.toml") in
      let* () = assert_exists Path.(workspace_root / Path.v ".riot" / Path.v "config.toml") in
      let* () = assert_exists Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit") in
      let* () = assert_executable Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit") in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-app"
          / Path.v "tests"
          / Path.v test_file)
      in
      let* () =
        assert_contains Path.(workspace_root / Path.v "README.md") ".github/workflows/ci.yml"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v ".agents"
          / Path.v "skills"
          / Path.v "riot"
          / Path.v "SKILL.md")
          "riot build"
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "config" / Path.v "dev.toml")
          {|name = "demo-app"|}
      in
      let* () =
        assert_contains Path.(workspace_root / Path.v ".riot" / Path.v "config.toml") "[riot.cache]"
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit")
          "riot test --small"
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "README.md")
          "riot new --lib ./packages/my-new-library"
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "README.md")
          "riot new --bin ./packages/my-new-binary"
      in
      let* () = assert_contains Path.(workspace_root / Path.v "Dockerfile") "RUN riot test" in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml")
          "uses: leostera/riot/docker/setup-riot@main"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-app"
          / Path.v "src"
          / Path.v (module_file_stem module_name ^ ".ml"))
          "let hello = fun () -> \"Hello from demo-app\""
      in
      assert_contains
        Path.(workspace_root
        / Path.v "packages"
        / Path.v "demo-app"
        / Path.v "tests"
        / Path.v test_file)
        (module_name ^ ".hello ()"))

let test_init_scaffolds_binary_workspace = fun _ctx ->
  with_tempdir_result
    "riot_init_bin"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "demo-bin") in
      let* () = run_init [ "init"; Path.to_string workspace_root; "--bin" ] in
      let module_name =
        Riot_model.Module_name.(from_string "demo-bin"
        |> to_string)
      in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
      in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v ".agents"
          / Path.v "skills"
          / Path.v "riot"
          / Path.v "SKILL.md")
      in
      let* () = assert_exists Path.(workspace_root / Path.v "config" / Path.v "dev.toml") in
      let* () = assert_exists Path.(workspace_root / Path.v ".riot" / Path.v "config.toml") in
      let* () = assert_exists Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit") in
      let* () = assert_executable Path.(workspace_root / Path.v ".githooks" / Path.v "pre-commit") in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v (module_file_stem module_name ^ ".ml"))
      in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "tests"
          / Path.v test_file)
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "Dockerfile")
          "ENTRYPOINT [\"/usr/local/bin/demo-bin\"]"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          "Std.Config.load ()"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          "Std.Log.set_level Info"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          "Std.Log.start_link ()"
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          (module_name ^ ".hello ()")
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          "let main ~args:_ ="
      in
      let* () =
        assert_contains
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "demo-bin"
          / Path.v "src"
          / Path.v "main.ml")
          "let () = Runtime.run ~main ~args:Env.args ()"
      in
      assert_contains
        Path.(workspace_root
        / Path.v "packages"
        / Path.v "demo-bin"
        / Path.v "tests"
        / Path.v test_file)
        "starter greeting")

let test_init_dot_scaffolds_current_directory = fun _ctx ->
  with_tempdir_result
    "riot_init_dot"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "agents-ml") in
      let* () =
        Fs.create_dir_all Path.(workspace_root / Path.v ".git")
        |> Result.map_err ~fn:IO.error_message
      in
      let* events =
        with_current_dir_result workspace_root (fun () -> run_init_with_events [ "init"; "." ])
      in
      let module_name =
        Riot_model.Module_name.(from_string "agents-ml"
        |> to_string)
      in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
      let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "agents-ml"|} in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "agents-ml"
          / Path.v "tests"
          / Path.v test_file)
      in
      let* () = assert_exists Path.(workspace_root / Path.v "Dockerfile") in
      match completion_event events with
      | Some (Riot_init.WorkspaceInitializationCompleted { next_steps; package_hints }) ->
          let* () =
            if List.any next_steps ~fn:(fun step -> String.starts_with ~prefix:"cd " step) then
              Error "expected init . completion to omit cd guidance"
            else
              Ok ()
          in
          let* () =
            if next_steps = [ "riot build"; "riot test" ] then
              Ok ()
            else
              Error "expected init . completion next steps to stay in place"
          in
          if
            package_hints
            = [
              (Riot_init.Library, "riot new --lib ./packages/<name>");
              (Riot_init.Binary, "riot new --bin ./packages/<name>");
            ]
          then
            Ok ()
          else
            Error "expected init completion to advertise library and binary package hints"
      | Some _ -> Error "expected final init event to be completion"
      | None -> Error "expected init completion event")

let test_init_without_path_defaults_to_current_directory = fun _ctx ->
  with_tempdir_result
    "riot_init_default_dot"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "default-dot") in
      let* () =
        Fs.create_dir_all workspace_root
        |> Result.map_err ~fn:IO.error_message
      in
      let* events =
        with_current_dir_result workspace_root (fun () -> run_init_with_events [ "init" ])
      in
      let module_name =
        Riot_model.Module_name.(from_string "default-dot"
        |> to_string)
      in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
      let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "default-dot"|} in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "default-dot"
          / Path.v "tests"
          / Path.v test_file)
      in
      match completion_event events with
      | Some (Riot_init.WorkspaceInitializationCompleted { next_steps; _ }) ->
          if List.any next_steps ~fn:(fun step -> String.starts_with ~prefix:"cd " step) then
            Error "expected init without a path to stay in the current directory"
          else if next_steps = [ "riot build"; "riot test" ] then
            Ok ()
          else
            Error "expected init without a path to keep local next steps"
      | Some _ -> Error "expected final init event to be completion"
      | None -> Error "expected init completion event")

let test_init_preserves_dotted_workspace_names = fun _ctx ->
  with_tempdir_result
    "riot_init_dotted_name"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "arewedown.dev") in
      let* () = run_init [ "init"; Path.to_string workspace_root; "--bin" ] in
      let module_name =
        Riot_model.Module_name.(from_string "arewedown-dev"
        |> to_string)
      in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
      let* () =
        assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "arewedown.dev"|}
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "riot.toml")
          {|members = [
  "packages/arewedown-dev"|}
      in
      let* () =
        assert_exists
          Path.(workspace_root / Path.v "packages" / Path.v "arewedown-dev" / Path.v "riot.toml")
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "packages" / Path.v "arewedown-dev" / Path.v "riot.toml")
          {|name = "arewedown-dev"|}
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "packages" / Path.v "arewedown-dev" / Path.v "riot.toml")
          {|[[bin]]
name = "arewedown-dev"|}
      in
      let* () =
        assert_exists
          Path.(workspace_root
          / Path.v "packages"
          / Path.v "arewedown-dev"
          / Path.v "tests"
          / Path.v test_file)
      in
      let* () = assert_contains Path.(workspace_root / Path.v "README.md") "riot run arewedown-dev" in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "Dockerfile")
          "/app/_build/release/*/arewedown-dev"
      in
      assert_contains
        Path.(workspace_root / Path.v "Dockerfile")
        {|ENTRYPOINT ["/usr/local/bin/arewedown-dev"]|})

let test_new_package_uses_typed_paths = fun _ctx ->
  with_tempdir_result
    "riot_init_new_package"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "workspace") in
      let* () = Result.map_err (Fs.create_dir_all workspace_root) ~fn:IO.error_message in
      let* () =
        Result.map_err
          (Fs.write
            {|
[workspace]
members = [
  "packages/demo",
]
|}
            Path.(workspace_root / Path.v "riot.toml"))
          ~fn:IO.error_message
      in
      let workspace = Riot_model.Workspace_manifest.make ~root:workspace_root ~packages:[] () in
      let package_dir = Path.(workspace_root / Path.v "packages" / Path.v "demo-lib") in
      let* (_created_path, created_name) =
        Riot_init.new_package ~workspace ~path:package_dir ~name:"demo-lib" ~is_library:true
      in
      let module_name = package_module_name "demo-lib" in
      let* () = assert_exists Path.(package_dir / Path.v "riot.toml") in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".ml"))
      in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".mli"))
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "riot.toml")
          {|members = [
  "packages/demo",
  "packages/demo-lib",
]|}
      in
      if String.equal created_name "demo-lib" then
        Ok ()
      else
        Error "expected new_package to preserve the package name")

let test_new_package_updates_workspace_members_for_absolute_paths = fun _ctx ->
  with_tempdir_result
    "riot_init_new_package_abs"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "workspace") in
      let* () = Result.map_err (Fs.create_dir_all workspace_root) ~fn:IO.error_message in
      let* () =
        Result.map_err
          (Fs.write {|
[workspace]
members = []
|} Path.(workspace_root / Path.v "riot.toml"))
          ~fn:IO.error_message
      in
      let workspace = Riot_model.Workspace_manifest.make ~root:workspace_root ~packages:[] () in
      let package_dir = Path.(workspace_root / Path.v "packages" / Path.v "demo-lib") in
      let* (_created_path, created_name) =
        Riot_init.new_package ~workspace ~path:package_dir ~name:"demo-lib" ~is_library:true
      in
      let module_name = package_module_name "demo-lib" in
      let* () = assert_exists Path.(package_dir / Path.v "riot.toml") in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".ml"))
      in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".mli"))
      in
      let* () =
        assert_contains
          Path.(workspace_root / Path.v "riot.toml")
          {|members = [
  "packages/demo-lib",
]|}
      in
      if String.equal created_name "demo-lib" then
        Ok ()
      else
        Error "expected new_package to preserve the package name")

let test_new_standalone_package_scaffolds_a_detached_package = fun _ctx ->
  with_tempdir_result
    "riot_init_new_standalone"
    (fun tempdir ->
      let package_dir = Path.(tempdir / Path.v "demo-lib") in
      let* (_created_path, created_name) =
        Riot_init.new_standalone_package ~path:package_dir ~name:"demo-lib" ~is_library:true
      in
      let module_name = package_module_name "demo-lib" in
      let* () = assert_exists Path.(package_dir / Path.v "riot.toml") in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".ml"))
      in
      let* () =
        assert_exists
          Path.(package_dir / Path.v "src" / Path.v (module_file_stem module_name ^ ".mli"))
      in
      if String.equal created_name "demo-lib" then
        Ok ()
      else
        Error "expected new_standalone_package to preserve the package name")

let tests =
  Test.[
    case
      "init scaffolds Docker, CI, and a starter test for libraries"
      test_init_scaffolds_library_workspace;
    case
      "init scaffolds Docker, CI, and a starter test for binaries"
      test_init_scaffolds_binary_workspace;
    case
      "init . scaffolds the current directory and records the workspace name"
      test_init_dot_scaffolds_current_directory;
    case
      "init defaults to the current directory when no path is passed"
      test_init_without_path_defaults_to_current_directory;
    case
      "init preserves dotted workspace names and normalizes the starter package"
      test_init_preserves_dotted_workspace_names;
    case
      "new_package scaffolds a package from a typed path and updates workspace members"
      test_new_package_uses_typed_paths;
    case
      "new_package normalizes absolute paths back into workspace members"
      test_new_package_updates_workspace_members_for_absolute_paths;
    case
      "new_standalone_package scaffolds a detached package from a typed path"
      test_new_standalone_package_scaffolds_a_detached_package;
  ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot_init_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
