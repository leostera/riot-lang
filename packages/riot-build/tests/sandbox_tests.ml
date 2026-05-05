open Std
open Riot_build
open Riot_model

module Test = Std.Test
module Sandbox = Riot_build.Internal.Sandbox

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
      Sandbox.prepare
        ~sandbox
        ~package
        ~inputs:[ Path.v "src/lib.ml" ]
        ~depset:[]
        ~store:(Riot_store.Store.create ~workspace);
      let copied = Path.(Sandbox.get_dir sandbox / Path.v "src/lib.ml") in
      let result =
        match Fs.read_to_string copied with
        | Ok content when String.equal content "let answer = 42" -> Ok ()
        | Ok content -> Error ("unexpected copied content: " ^ content)
        | Error err -> Error ("failed to read copied input: " ^ IO.error_message err)
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

let tests =
  Test.[
    case "sandbox create makes a directory" test_sandbox_create_and_get_dir;
    case "sandbox prepare copies package inputs" test_sandbox_prepare_copies_package_inputs;
    case "sandbox cleanup removes directory" test_sandbox_cleanup_removes_dir;
    case "sandbox create uses workspace target_dir_root" test_sandbox_uses_workspace_target_dir_root;
  ]

let name = "riot-build:sandbox"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
