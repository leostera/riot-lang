open Std
module Test = Std.Test

let ( let* ) = Result.and_then

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
  let* () = Riot_init.run ~on_event:(fun event -> events := event :: !events) matches
  |> Result.map_error Exception.to_string in
  Ok (List.rev !events)

let assert_exists = fun path ->
  match Fs.exists path with
  | Ok true -> Ok ()
  | Ok false -> Error ("expected file to exist: " ^ Path.to_string path)
  | Error err -> Error (IO.error_message err)

let assert_contains = fun path needle ->
  let* source = Fs.read path |> Result.map_error IO.error_message in
  if String.contains source needle then
    Ok ()
  else
    Error ("expected " ^ Path.to_string path ^ " to contain: " ^ needle)

let path_error_to_string = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> syscall ^ ": invalid UTF-8 path: " ^ path
  | Path.SystemError message -> message

let with_current_dir_result = fun dir fn ->
  let original = Env.current_dir () |> Result.expect ~msg:"expected current directory" in
  let result =
    match Env.set_current_dir dir with
    | Ok () -> fn ()
    | Error err -> Error (path_error_to_string err)
  in
  match Env.set_current_dir original with
  | Error err -> Error (path_error_to_string err)
  | Ok () -> result

let completion_event = fun events ->
  events |> List.rev |> List.find_opt
    (
      function
      | Riot_init.WorkspaceInitializationCompleted _ -> true
      | _ -> false
    )

let test_init_scaffolds_library_workspace = fun _ctx ->
  with_tempdir_result "riot_init_lib"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "demo-app") in
      let* () = run_init [ "init"; Path.to_string workspace_root ] in
      let module_name = Riot_model.Module_name.(of_string "demo-app" |> to_string) in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "Dockerfile") in
      let* () = assert_exists
        Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml") in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "demo-app" / Path.v "tests" / Path.v test_file) in
      let* () = assert_contains Path.(workspace_root / Path.v "README.md") ".github/workflows/ci.yml" in
      let* () = assert_contains Path.(workspace_root / Path.v "README.md") "riot new --lib ./packages/my-new-library" in
      let* () = assert_contains Path.(workspace_root / Path.v "README.md") "riot new --bin ./packages/my-new-binary" in
      let* () = assert_contains Path.(workspace_root / Path.v "Dockerfile") "RUN riot test" in
      let* () = assert_contains
        Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml")
        "uses: leostera/riot/docker/setup-riot@main" in
      let* () = assert_contains
        Path.(workspace_root
        / Path.v "packages"
        / Path.v "demo-app"
        / Path.v "src"
        / Path.v (module_name ^ ".ml"))
        "let hello = fun () -> \"Hello from demo-app\"" in
      assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-app" / Path.v "tests" / Path.v test_file)
        (module_name ^ ".hello ()"))

let test_init_scaffolds_binary_workspace = fun _ctx ->
  with_tempdir_result "riot_init_bin"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "demo-bin") in
      let* () = run_init [ "init"; Path.to_string workspace_root; "--bin" ] in
      let module_name = Riot_model.Module_name.(of_string "demo-bin" |> to_string) in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "src" / Path.v "main.ml") in
      let* () = assert_exists
        Path.(workspace_root
        / Path.v "packages"
        / Path.v "demo-bin"
        / Path.v "src"
        / Path.v (module_name ^ ".ml")) in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "tests" / Path.v test_file) in
      let* () = assert_contains Path.(workspace_root / Path.v "Dockerfile") "ENTRYPOINT [\"/usr/local/bin/demo-bin\"]" in
      let* () = assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "src" / Path.v "main.ml")
        (module_name ^ ".hello ()") in
      assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "tests" / Path.v test_file)
        "starter greeting")

let test_init_dot_scaffolds_current_directory = fun _ctx ->
  with_tempdir_result "riot_init_dot"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "agents-ml") in
      let* () = Fs.create_dir_all Path.(workspace_root / Path.v ".git") |> Result.map_error IO.error_message in
      let* events =
        with_current_dir_result workspace_root (fun () -> run_init_with_events [ "init"; "." ])
      in
      let module_name = Riot_model.Module_name.(of_string "agents-ml" |> to_string) in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
      let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "agents-ml"|} in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "agents-ml" / Path.v "tests" / Path.v test_file) in
      let* () = assert_exists Path.(workspace_root / Path.v "Dockerfile") in
      match completion_event events with
      | Some (Riot_init.WorkspaceInitializationCompleted { next_steps; package_hints }) ->
          let* () =
            if List.exists (fun step -> String.starts_with ~prefix:"cd " step) next_steps then
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
      | Some _ ->
          Error "expected final init event to be completion"
      | None ->
          Error "expected init completion event")

let test_init_without_path_defaults_to_current_directory = fun _ctx ->
  with_tempdir_result "riot_init_default_dot"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "default-dot") in
      let* () = Fs.create_dir_all workspace_root |> Result.map_error IO.error_message in
      let* events =
        with_current_dir_result workspace_root (fun () -> run_init_with_events [ "init" ])
      in
      let module_name = Riot_model.Module_name.(of_string "default-dot" |> to_string) in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "riot.toml") in
      let* () = assert_contains Path.(workspace_root / Path.v "riot.toml") {|name = "default-dot"|} in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "default-dot" / Path.v "tests" / Path.v test_file) in
      match completion_event events with
      | Some (Riot_init.WorkspaceInitializationCompleted { next_steps; _ }) ->
          if List.exists (fun step -> String.starts_with ~prefix:"cd " step) next_steps then
            Error "expected init without a path to stay in the current directory"
          else if next_steps = [ "riot build"; "riot test" ] then
            Ok ()
          else
            Error "expected init without a path to keep local next steps"
      | Some _ -> Error "expected final init event to be completion"
      | None -> Error "expected init completion event")

let tests =
  Test.[
    case "init scaffolds Docker, CI, and a starter test for libraries" test_init_scaffolds_library_workspace;
    case "init scaffolds Docker, CI, and a starter test for binaries" test_init_scaffolds_binary_workspace;
    case "init . scaffolds the current directory and records the workspace name" test_init_dot_scaffolds_current_directory;
    case "init defaults to the current directory when no path is passed" test_init_without_path_defaults_to_current_directory;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"riot_init_tests" ~tests ~args) ~args:Env.args ()
