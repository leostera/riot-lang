open Std

module Test = Std.Test

let make_package tmpdir name content =
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in

  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write content ml_file |> Result.expect ~msg:"Write ml failed" in

  let tusk_file = Path.(pkg_dir / Path.v "tusk.toml") in
  let tusk_content =
    format
      "[package]\n\
       name = \"%s\"\n\
       version = \"0.0.1\"\n\n\
       [lib]\n\
       path = \"src/lib.ml\"\n"
      name
  in
  let _ =
    Fs.write tusk_content tusk_file |> Result.expect ~msg:"Write tusk.toml"
  in

  Tusk_model.Package.
    {
      name;
      path = pkg_dir;
      relative_path = Path.v name;
      dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources = { src = []; native = []; tests = []; examples = [] };
    }

type Message.t += BuildComplete of (string * (unit, string) result)

type Message.t +=
  | BuildCompleteWithCache of (string * bool * (unit, string) result)

let test_concurrent_builds_different_packages () =
  match
    Fs.with_tempdir ~prefix:"concurrent_test" (fun tmpdir ->
        let pkg1 = make_package tmpdir "pkg-1" "let x = 1" in
        let pkg2 = make_package tmpdir "pkg-2" "let x = 2" in
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [ pkg1; pkg2 ];
            }
        in
        let toolchain =
          Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
          |> Result.expect ~msg:"Failed to initialize toolchain"
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create workspace |> Result.unwrap in

        let parent = self () in

        let _worker1 =
          spawn (fun () ->
              let result =
                Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                  ~package_graph ~package:pkg1
              in
              let status =
                match result.status with
                | Built _ | Cached _ -> Ok ()
                | Failed err ->
                    Error
                      (match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message)
              in
              send parent (BuildComplete ("pkg-1", status));
              Ok ())
        in

        let _worker2 =
          spawn (fun () ->
              let result =
                Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                  ~package_graph ~package:pkg2
              in
              let status =
                match result.status with
                | Built _ | Cached _ -> Ok ()
                | Failed err ->
                    Error
                      (match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message)
              in
              send parent (BuildComplete ("pkg-2", status));
              Ok ())
        in

        let selector msg =
          match msg with BuildComplete _ -> `select msg | _ -> `skip
        in

        let result1 = receive ~selector () in
        let result2 = receive ~selector () in

        match (result1, result2) with
        | BuildComplete (name1, Ok ()), BuildComplete (name2, Ok ()) ->
            if
              (name1 = "pkg-1" && name2 = "pkg-2")
              || (name1 = "pkg-2" && name2 = "pkg-1")
            then Ok ()
            else Error (format "Unexpected package names: %s, %s" name1 name2)
        | BuildComplete (name, Error err), _ ->
            Error (format "%s build failed: %s" name err)
        | _, BuildComplete (name, Error err) ->
            Error (format "%s build failed: %s" name err)
        | _ -> Error "Unexpected message")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_same_package () =
  match
    Fs.with_tempdir ~prefix:"concurrent_test" (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [ package ];
            }
        in
        let toolchain =
          Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
          |> Result.expect ~msg:"Failed to initialize toolchain"
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create workspace |> Result.unwrap in

        let parent = self () in

        let _worker1 =
          spawn (fun () ->
              let result =
                Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                  ~package_graph ~package
              in
              let status =
                match result.status with
                | Built _ | Cached _ -> Ok ()
                | Failed err ->
                    Error
                      (match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message)
              in
              send parent (BuildComplete ("worker1", status));
              Ok ())
        in

        let _worker2 =
          spawn (fun () ->
              let result =
                Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                  ~package_graph ~package
              in
              let status =
                match result.status with
                | Built _ | Cached _ -> Ok ()
                | Failed err ->
                    Error
                      (match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message)
              in
              send parent (BuildComplete ("worker2", status));
              Ok ())
        in

        let selector msg =
          match msg with BuildComplete _ -> `select msg | _ -> `skip
        in

        let result1 = receive ~selector () in
        let result2 = receive ~selector () in

        match (result1, result2) with
        | BuildComplete (_, Ok ()), BuildComplete (_, Ok ()) -> Ok ()
        | BuildComplete (name, Error err), _ ->
            Error (format "%s build failed: %s" name err)
        | _, BuildComplete (name, Error err) ->
            Error (format "%s build failed: %s" name err)
        | _ -> Error "Unexpected message")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_with_shared_cache () =
  match
    Fs.with_tempdir ~prefix:"concurrent_test" (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [ package ];
            }
        in
        let toolchain =
          Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
          |> Result.expect ~msg:"Failed to initialize toolchain"
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create workspace |> Result.unwrap in

        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
            ~package_graph ~package
        in

        match first_build.status with
        | Built _ -> (
            let parent = self () in

            let _worker1 =
              spawn (fun () ->
                  let result =
                    Tusk_executor.Package_builder.build ~workspace ~toolchain
                      ~store ~package_graph ~package
                  in
                  let cached =
                    match result.status with
                    | Cached _ -> true
                    | Built _ -> false
                    | Failed _ -> false
                  in
                  let status =
                    match result.status with
                    | Cached _ | Built _ -> Ok ()
                    | Failed err ->
                        Error
                          (match err with
                          | PlanningFailed _ -> "planning"
                          | ExecutionFailed { message } -> message)
                  in
                  send parent
                    (BuildCompleteWithCache ("worker1", cached, status));
                  Ok ())
            in

            let _worker2 =
              spawn (fun () ->
                  let result =
                    Tusk_executor.Package_builder.build ~workspace ~toolchain
                      ~store ~package_graph ~package
                  in
                  let cached =
                    match result.status with
                    | Cached _ -> true
                    | Built _ -> false
                    | Failed _ -> false
                  in
                  let status =
                    match result.status with
                    | Cached _ | Built _ -> Ok ()
                    | Failed err ->
                        Error
                          (match err with
                          | PlanningFailed _ -> "planning"
                          | ExecutionFailed { message } -> message)
                  in
                  send parent
                    (BuildCompleteWithCache ("worker2", cached, status));
                  Ok ())
            in

            let selector msg =
              match msg with
              | BuildCompleteWithCache _ -> `select msg
              | _ -> `skip
            in

            let result1 = receive ~selector () in
            let result2 = receive ~selector () in

            match (result1, result2) with
            | ( BuildCompleteWithCache (_, cached1, Ok ()),
                BuildCompleteWithCache (_, cached2, Ok ()) ) ->
                if cached1 && cached2 then Ok ()
                else
                  Error
                    (format
                       "Expected both builds to be cached, got: worker1=%b, \
                        worker2=%b"
                       cached1 cached2)
            | BuildCompleteWithCache (name, _, Error err), _ ->
                Error (format "%s build failed: %s" name err)
            | _, BuildCompleteWithCache (name, _, Error err) ->
                Error (format "%s build failed: %s" name err)
            | _ -> Error "Unexpected message")
        | Cached _ -> Error "First build should not be cached"
        | Failed err ->
            Error
              (format "First build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning"
                 | ExecutionFailed { message } -> message)))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  let open Test in
  [
    case "concurrent: different packages don't interfere"
      test_concurrent_builds_different_packages;
    case "concurrent: same package builds safely"
      test_concurrent_builds_same_package;
    case "concurrent: shared cache works correctly"
      test_concurrent_builds_with_shared_cache;
  ]

let name = "Concurrent Build Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
