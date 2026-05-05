open Std
open Riot_build
open Riot_model

module Test = Std.Test
module Sandbox = Riot_build.Internal.Sandbox

type Message.t +=
  | Sandbox_paths_created of (Path.t list, string) result

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let make_workspace = fun root ->
  Riot_model.Workspace.make ~root ~target_dir:"target" ~packages:[] ()

let make_package = fun ~root ~name ->
  let package_name = package_name name in
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name:package_name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let test_sandbox_create_and_get_dir = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_create"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let sandbox = Sandbox.create ~workspace () ~package_name:(package_name "pkg") in
      let dir = Sandbox.get_dir sandbox in
      let exists =
        Fs.exists dir
        |> Result.unwrap_or ~default:false
      in
      let _ = Sandbox.cleanup sandbox in
      if exists then
        Ok ()
      else
        Error "expected sandbox directory to exist") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_sandbox_prepare_copies_package_inputs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_prepare"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let package_src = Path.(package.Riot_model.Package.path / Path.v "src") in
      let _ =
        Fs.create_dir_all package_src
        |> Result.expect ~msg:"create package src failed"
      in
      let source = Path.(package_src / Path.v "lib.ml") in
      let _ =
        Fs.write "let answer = 42" source
        |> Result.expect ~msg:"write package source failed"
      in
      let sandbox = Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
      let stats =
        Sandbox.prepare
          ~sandbox
          ~package
          ~inputs:[ Path.v "src/lib.ml" ]
          ~depset:[]
          ~store:(Riot_store.Store.create ~workspace)
        |> Result.expect ~msg:"sandbox prepare should copy package inputs"
      in
      let copied = Path.(Sandbox.get_dir sandbox / Path.v "src/lib.ml") in
      let result =
        if not (Int.equal stats.input_count 1) then
          Error ("expected one copied input, got " ^ Int.to_string stats.input_count)
        else
          match Fs.read_to_string copied with
        | Ok content when String.equal content "let answer = 42" -> Ok ()
        | Ok content -> Error ("unexpected copied content: " ^ content)
        | Error err -> Error ("failed to read copied input: " ^ IO.error_message err)
      in
      let _ = Sandbox.cleanup sandbox in
      result) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let make_dependency = fun
  ~package ~artifact_dir ?input_hash ?output_hash () ->
  let input_hash =
    input_hash
    |> Option.unwrap_or ~default:(Crypto.hash_string (Package_name.to_string package.Package.name ^ ":input"))
  in
  let output_hash =
    output_hash
    |> Option.unwrap_or ~default:(Crypto.hash_string (Package_name.to_string package.Package.name ^ ":output"))
  in
  Riot_planner.Dependency.{
    package;
    artifact_dir;
    depset = [];
    input_hash;
    output_hash;
  }

let test_sandbox_copies_dependency_object_files = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_dependency_objects"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let dependency_package = make_package ~root:tmpdir ~name:"dep" in
      let staging_dir = Path.(tmpdir / Path.v "dep-artifact-staging") in
      let _ =
        Fs.create_dir_all staging_dir
        |> Result.expect ~msg:"create dependency artifact dir failed"
      in
      let _ =
        Fs.write "object" Path.(staging_dir / Path.v "dep_runtime.o")
        |> Result.expect ~msg:"write dependency object failed"
      in
      let _ =
        Fs.write "interface" Path.(staging_dir / Path.v "Dep.cmi")
        |> Result.expect ~msg:"write dependency interface failed"
      in
      let input_hash = Crypto.hash_string "dep-input" in
      let artifact =
        Riot_store.Store.save_package
          store
          ~package:"dep"
          ~input_hash
          ~sandbox_dir:staging_dir
          ~outs:[ Path.(staging_dir / Path.v "dep_runtime.o"); Path.(staging_dir / Path.v "Dep.cmi") ]
        |> Result.expect ~msg:"save dependency package artifact failed"
      in
      let artifact_dir = Riot_store.Store.hash_dir_of store input_hash in
      let sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
      let dependency =
        make_dependency
          ~package:dependency_package
          ~artifact_dir
          ~input_hash
          ~output_hash:artifact.Riot_store.Artifact.output_hash
          ()
      in
      let result =
        match
          Sandbox.copy_dependency_object_files
            ~store
            ~sandbox
            ~package
            ~depset:[ dependency ]
        with
        | Error err -> Error (Sandbox.dependency_copy_error_to_string err)
        | Ok stats ->
            let copied_object = Path.(Sandbox.get_dir sandbox / Path.v "dep_runtime.o") in
            let copied_interface = Path.(Sandbox.get_dir sandbox / Path.v "Dep.cmi") in
            let object_exists = Fs.exists copied_object |> Result.unwrap_or ~default:false in
            let interface_exists = Fs.exists copied_interface |> Result.unwrap_or ~default:true in
            if not (Int.equal stats.dependency_count 1) then
              Error ("expected one dependency, got " ^ Int.to_string stats.dependency_count)
            else if not (Int.equal stats.object_count 1) then
              Error ("expected one dependency object, got " ^ Int.to_string stats.object_count)
            else if not object_exists then
              Error "expected dependency object to be copied into sandbox"
            else if interface_exists then
              Error "expected non-object dependency files not to be copied into sandbox"
            else
              Ok ()
      in
      let _ = Sandbox.cleanup sandbox in
      result) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_sandbox_fails_when_dependency_artifact_dir_is_missing = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_missing_dependency_artifact"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let dependency_package = make_package ~root:tmpdir ~name:"dep" in
      let missing_artifact_dir = Path.(tmpdir / Path.v "missing-artifact") in
      let sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
      let dependency = make_dependency ~package:dependency_package ~artifact_dir:missing_artifact_dir () in
      let result =
        match
          Sandbox.copy_dependency_object_files
            ~store:(Riot_store.Store.create ~workspace)
            ~sandbox
            ~package
            ~depset:[ dependency ]
        with
        | Ok _ -> Error "expected missing dependency artifact dir to fail sandbox preparation"
        | Error (Sandbox.DependencyArtifactDirUnavailable { package = failed_package; artifact_dir; _ }) ->
            if not (Package_name.equal failed_package dependency_package.Package.name) then
              Error "expected failure to identify dependency package"
            else if not (Path.equal artifact_dir missing_artifact_dir) then
              Error "expected failure to identify missing artifact directory"
            else
              Ok ()
        | Error err -> Error ("unexpected dependency copy error: " ^ Sandbox.dependency_copy_error_to_string err)
      in
      let _ = Sandbox.cleanup sandbox in
      result) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_sandbox_cleanup_removes_dir = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_cleanup"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let sandbox = Sandbox.create ~workspace () ~package_name:(package_name "pkg") in
      let dir = Sandbox.get_dir sandbox in
      let _ = Sandbox.cleanup sandbox in
      let exists =
        Fs.exists dir
        |> Result.unwrap_or ~default:true
      in
      if not exists then
        Ok ()
      else
        Error "expected sandbox cleanup to remove directory") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_sandbox_uses_workspace_target_dir_root = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_custom_target"
    (fun tmpdir ->
      let workspace =
        Riot_model.Workspace.make ~root:tmpdir ~target_dir:"build-out" ~packages:[] ()
      in
      let sandbox = Sandbox.create ~workspace () ~package_name:(package_name "pkg") in
      let dir =
        Sandbox.get_dir sandbox
        |> Path.to_string
      in
      let expected_prefix = Path.to_string workspace.target_dir_root in
      let _ = Sandbox.cleanup sandbox in
      if String.starts_with ~prefix:expected_prefix dir then
        Ok ()
      else
        Error ("expected sandbox under " ^ expected_prefix ^ ", got " ^ dir)) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let assert_unique_paths = fun paths ->
  let seen = Collections.HashSet.with_capacity ~size:(List.length paths) in
  let rec loop count paths =
    match paths with
    | [] -> Ok count
    | path :: rest ->
        let value = Path.to_string path in
        if Collections.HashSet.insert seen ~value then
          loop (count + 1) rest
        else
          Error ("duplicate sandbox path: " ^ value)
  in
  loop 0 paths

let receive_sandbox_paths = fun ~actor_count ->
  let rec collect remaining acc =
    if remaining = 0 then
      Ok (List.concat (List.reverse acc))
    else
      (
        match
          receive
            ~timeout:(Time.Duration.from_secs 5)
            ~selector:(fun __tmp1 ->
              match __tmp1 with
              | Sandbox_paths_created result -> Select result
              | _ -> Skip)
            ()
        with
        | exception Receive_timeout -> Error "timed out waiting for sandbox workers"
        | Error message -> Error message
        | Ok paths -> collect (remaining - 1) (paths :: acc)
      )
  in
  collect actor_count []

let test_sandbox_create_uses_unique_seeded_dirs_under_concurrency = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"sandbox_unique_dirs"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let package_name = package_name "pkg" in
      let session_id = Session_id.from_string "sandbox-seed-test" in
      let parent = self () in
      let actor_count = 10 in
      let sandboxes_per_actor = 100 in
      for actor_index = 0 to actor_count - 1 do
        ignore
          (
            Actor.spawn
              (fun () ->
                let result =
                  let rec create_paths sandbox_index acc =
                    if sandbox_index = sandboxes_per_actor then
                      List.reverse acc
                    else
                      (
                        let id_seed =
                          Crypto.hash_string
                            (
                              Int.to_string actor_index
                              ^ ":"
                              ^ Int.to_string sandbox_index
                            )
                        in
                        let sandbox =
                          Sandbox.create
                            ~workspace
                            ~id_seed
                            ~session_id
                            ()
                            ~package_name
                        in
                        create_paths (sandbox_index + 1) (Sandbox.get_dir sandbox :: acc)
                      )
                  in
                  match create_paths 0 [] with
                  | exception exn -> Error (Exception.to_string exn)
                  | paths -> Ok paths
                in
                send parent (Sandbox_paths_created result);
                Ok ())
          )
      done;
      match receive_sandbox_paths ~actor_count with
      | Error _ as err -> err
      | Ok paths -> (
          match assert_unique_paths paths with
          | Error _ as err -> err
          | Ok count ->
              let expected = actor_count * sandboxes_per_actor in
              if count = expected then
                Ok ()
              else
                Error
                  (
                    "expected "
                    ^ Int.to_string expected
                    ^ " sandbox paths, got "
                    ^ Int.to_string count
                  )
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case "sandbox create makes a directory" test_sandbox_create_and_get_dir;
    case "sandbox prepare copies package inputs" test_sandbox_prepare_copies_package_inputs;
    case "sandbox copies dependency object files" test_sandbox_copies_dependency_object_files;
    case
      "sandbox fails when dependency artifact dir is missing"
      test_sandbox_fails_when_dependency_artifact_dir_is_missing;
    case "sandbox cleanup removes directory" test_sandbox_cleanup_removes_dir;
    case "sandbox create uses workspace target_dir_root" test_sandbox_uses_workspace_target_dir_root;
    case
      "sandbox create uses unique seeded dirs under concurrency"
      test_sandbox_create_uses_unique_seeded_dirs_under_concurrency;
  ]

let name = "riot-build:sandbox"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
