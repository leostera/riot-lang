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

let run_init = fun args ->
  let* matches = parse_init args in
  Riot_init.run matches |> Result.map_error Exception.to_string

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

let test_init_scaffolds_library_workspace = fun _ctx ->
  with_tempdir_result "riot_init_lib"
    (fun tempdir ->
      let workspace_root = Path.(tempdir / Path.v "demo-app") in
      let* () = run_init [ "init"; Path.to_string workspace_root ] in
      let module_name = Riot_model.Module_name.(of_string "demo-app" |> to_string) in
      let test_file = String.lowercase_ascii module_name ^ "_tests.ml" in
      let* () = assert_exists Path.(workspace_root / Path.v "Dockerfile") in
      let* () = assert_exists Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml") in
      let* () = assert_exists Path.(workspace_root / Path.v "packages" / Path.v "demo-app" / Path.v "tests" / Path.v test_file) in
      let* () = assert_contains Path.(workspace_root / Path.v "README.md") ".github/workflows/ci.yml" in
      let* () = assert_contains Path.(workspace_root / Path.v "Dockerfile") "RUN riot test" in
      let* () = assert_contains
        Path.(workspace_root / Path.v ".github" / Path.v "workflows" / Path.v "ci.yml")
        "uses: leostera/riot/docker/setup-riot@main"
      in
      let* () = assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-app" / Path.v "src" / Path.v (module_name ^ ".ml"))
        "let hello = fun () -> \"Hello from demo-app\""
      in
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
      let* () = assert_exists Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "src" / Path.v "main.ml") in
      let* () = assert_exists
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "src" / Path.v (module_name ^ ".ml"))
      in
      let* () = assert_exists Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "tests" / Path.v test_file) in
      let* () = assert_contains Path.(workspace_root / Path.v "Dockerfile") "ENTRYPOINT [\"/usr/local/bin/demo-bin\"]" in
      let* () = assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "src" / Path.v "main.ml")
        (module_name ^ ".hello ()")
      in
      assert_contains
        Path.(workspace_root / Path.v "packages" / Path.v "demo-bin" / Path.v "tests" / Path.v test_file)
        "starter greeting")

let tests =
  Test.[
    case "init scaffolds Docker, CI, and a starter test for libraries" test_init_scaffolds_library_workspace;
    case "init scaffolds Docker, CI, and a starter test for binaries" test_init_scaffolds_binary_workspace;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"riot_init_tests" ~tests ~args) ~args:Env.args ()
