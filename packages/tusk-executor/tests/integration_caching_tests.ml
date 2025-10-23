open Std
open Miniriot
module Test = Std.Test

let test_toolchain =
  lazy
    (Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize test toolchain")

let make_test_workspace tmpdir =
  Tusk_model.Workspace.
    {
      root = tmpdir;
      target_dir_root = Path.(tmpdir / Path.v "target");
      packages = [];
    }

let make_simple_package tmpdir name =
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in

  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ =
    Fs.write "let x = 42" ml_file |> Result.expect ~msg:"Write ml failed"
  in

  Tusk_model.Package.
    {
      name;
      path = pkg_dir;
      relative_path = Path.v name;
      dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      test_library = None;
      test_modules = [];
    }

let test_package_cache_hit_skips_planning () =
  match
    Fs.with_tempdir ~prefix:"pkg_cache_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let toolchain = Lazy.force test_toolchain in
        let package = make_simple_package tmpdir "test_pkg" in
        let package_graph = Tusk_planner.Package_graph.create workspace in

        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
            ~package_graph ~package
        in

        match first_build.status with
        | Built artifact -> (
            let second_build =
              Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                ~package_graph ~package
            in

            match second_build.status with
            | Cached cached_artifact ->
                if
                  Crypto.Digest.hex artifact.hash
                  = Crypto.Digest.hex cached_artifact.hash
                then Ok ()
                else Error "Cached artifact hash mismatch"
            | Built _ -> Error "Expected cache hit, got rebuild"
            | Failed err ->
                Error
                  (format "Second build failed: %s"
                     (match err with
                     | PlanningFailed _ -> "planning"
                     | ExecutionFailed { message } -> message)))
        | Failed err ->
            Error
              (format "First build failed: %s"
                 (match err with
                 | PlanningFailed _ -> "planning failed"
                 | ExecutionFailed { message } -> message))
        | Cached _ -> Error "First build should not be cached")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_package_cache_miss_on_source_change () =
  match
    Fs.with_tempdir ~prefix:"pkg_cache_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let toolchain = Lazy.force test_toolchain in
        let package = make_simple_package tmpdir "test_pkg" in
        let package_graph = Tusk_planner.Package_graph.create workspace in

        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
            ~package_graph ~package
        in

        let ml_file = Path.(package.path / Path.v "src" / Path.v "lib.ml") in
        let _ =
          Fs.write "let x = 99" ml_file
          |> Result.expect ~msg:"Write modified ml failed"
        in

        let package_modified = make_simple_package tmpdir "test_pkg" in

        let second_build =
          Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
            ~package_graph ~package:package_modified
        in

        match (first_build.status, second_build.status) with
        | (Built _ | Failed _), (Built _ | Failed _) -> Ok ()
        | _, Cached _ ->
            Error "Expected cache miss after source change, got cache hit"
        | Cached _, _ -> Error "First build should not be cached")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_action_hash_consistency () =
  let action =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "foo.mli";
        outputs = [ Path.v "foo.cmi" ];
        includes = [];
        flags = [];
      }
  in

  let hash1 = Tusk_executor.Action_executor.hash_action action in
  let hash2 = Tusk_executor.Action_executor.hash_action action in

  if Crypto.Digest.hex hash1 = Crypto.Digest.hex hash2 then Ok ()
  else Error "Action hash not consistent"

let test_action_cache_stores_output () =
  match
    Fs.with_tempdir ~prefix:"action_cache_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir
          |> Result.expect ~msg:"Create sandbox failed"
        in

        let output = Path.(sandbox_dir / Path.v "result.txt") in
        let _ =
          Fs.write "action output" output |> Result.expect ~msg:"Write failed"
        in

        let action =
          Tusk_planner.Action.WriteFile
            { destination = output; content = "action output" }
        in
        let action_hash = Tusk_executor.Action_executor.hash_action action in

        match
          Tusk_store.Store.save store ~package:"_action_cache" ~hash:action_hash
            ~sandbox_dir ~outs:[ output ]
        with
        | Ok _ -> (
            match Tusk_store.Store.get store action_hash with
            | Some artifact ->
                if
                  Crypto.Digest.hex artifact.hash
                  = Crypto.Digest.hex action_hash
                then Ok ()
                else Error "Action artifact hash mismatch"
            | None -> Error "Action artifact not found in store")
        | Error e -> Error (format "Failed to save action to store: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_action_cache_retrieval_and_promotion () =
  match
    Fs.with_tempdir ~prefix:"action_cache_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir
          |> Result.expect ~msg:"Create sandbox failed"
        in

        let output = Path.(sandbox_dir / Path.v "compiled.cmi") in
        let _ =
          Fs.write "compiled interface" output
          |> Result.expect ~msg:"Write failed"
        in

        let action =
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              outputs = [ Path.v "compiled.cmi" ];
              includes = [];
              flags = [];
            }
        in
        let action_hash = Tusk_executor.Action_executor.hash_action action in

        let _ =
          Tusk_store.Store.save store ~package:"_action_cache" ~hash:action_hash
            ~sandbox_dir ~outs:[ output ]
          |> Result.expect ~msg:"Save failed"
        in

        let new_sandbox = Path.(tmpdir / Path.v "new_sandbox") in
        let _ =
          Fs.create_dir_all new_sandbox
          |> Result.expect ~msg:"Create new sandbox failed"
        in

        match
          Tusk_store.Store.promote store action_hash ~target_dir:new_sandbox
        with
        | Ok () -> (
            let promoted_file = Path.(new_sandbox / Path.v "compiled.cmi") in
            match Fs.exists promoted_file with
            | Ok true -> (
                match Fs.read promoted_file with
                | Ok content ->
                    if String.equal content "compiled interface" then Ok ()
                    else Error "Promoted content mismatch"
                | Error _ -> Error "Failed to read promoted file")
            | Ok false -> Error "Promoted file not found"
            | Error _ -> Error "Failed to check promoted file")
        | Error e -> Error (format "Promotion failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  let open Test in
  [
    case "package cache: hit skips planning"
      test_package_cache_hit_skips_planning;
    case "package cache: miss on source change"
      test_package_cache_miss_on_source_change;
    case "action hash: consistency" test_action_hash_consistency;
    case "action cache: stores output" test_action_cache_stores_output;
    case "action cache: retrieval and promotion"
      test_action_cache_retrieval_and_promotion;
  ]

let name = "Integration Caching Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
