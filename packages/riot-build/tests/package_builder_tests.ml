open Std
open Riot_build
open Std.Collections
open Riot_model

module Package_builder = Riot_build.Internal.Package_builder
module Test = Std.Test

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let unit_key = fun package ->
  ({
    package = package.Riot_model.Package.name;
    artifact = Riot_planner.Build_unit.Library;
    target = Riot_model.Target.host ();
    profile = Riot_model.Profile.debug;
  }:Riot_planner.Build_unit.key)

let build_workspace_package = fun ~workspace package ->
  let request =
    Riot_build.Request.make
      ~workspace
      ~packages:[ package.Riot_model.Package.name ]
      ~targets:Riot_model.Target.Host
      ~scope:Riot_build.Request.Runtime
      ~profile:Riot_model.Profile.debug
      ()
  in
  match Riot_build.build request with
  | Error err -> Error (Riot_build.error_message err)
  | Ok result -> (
      match Riot_build.Build_result.find_package result package.name with
      | Some package_result -> Ok package_result
      | None ->
          Error ("expected package result for "
          ^ Riot_model.Package_name.to_string package.name)
    )

let workspace_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source =
      {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      };
  }

let test_collect_source_files = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"pkg_test"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let _ = Fs.create_dir_all src_dir in
      let ml_file = Path.(src_dir / Path.v "foo.ml") in
      let mli_file = Path.(src_dir / Path.v "bar.mli") in
      let c_file = Path.(src_dir / Path.v "native.c") in
      let txt_file = Path.(src_dir / Path.v "readme.txt") in
      let _ =
        Fs.write "let x = 1" ml_file
        |> Result.expect ~msg:"Write failed"
      in
      let _ =
        Fs.write "val x : int" mli_file
        |> Result.expect ~msg:"Write failed"
      in
      let _ =
        Fs.write "int main() {}" c_file
        |> Result.expect ~msg:"Write failed"
      in
      let _ =
        Fs.write "readme" txt_file
        |> Result.expect ~msg:"Write failed"
      in
      let package =
        Riot_model.Package.make
          ~name:(package_name "test")
          ~path:tmpdir
          ~relative_path:(Path.v ".")
          ()
      in
      let files = Package_builder.collect_source_files package in
      let has_ml = List.any files ~fn:(fun p -> Path.to_string p = Path.to_string ml_file) in
      let has_mli = List.any files ~fn:(fun p -> Path.to_string p = Path.to_string mli_file) in
      let has_c = List.any files ~fn:(fun p -> Path.to_string p = Path.to_string c_file) in
      let has_txt = List.any files ~fn:(fun p -> Path.to_string p = Path.to_string txt_file) in
      let count = List.length files in
      if has_ml && has_mli && has_c && (not has_txt) && count = 3 then
        Ok ()
      else
        Error ("Expected exactly .ml, .mli, .c files. Got "
        ^ Int.to_string count
        ^ " files: has_ml="
        ^ Bool.to_string has_ml
        ^ " has_mli="
        ^ Bool.to_string has_mli
        ^ " has_c="
        ^ Bool.to_string has_c
        ^ " has_txt="
        ^ Bool.to_string has_txt
        ^ ". Files: "
        ^ String.concat ", " (List.map files ~fn:(fun p -> Path.basename p)))) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_build_result_status_variants = fun _ctx ->
  let package =
    Riot_model.Package.make
      ~name:(package_name "test")
      ~path:(Path.v ".")
      ~relative_path:(Path.v ".")
      ()
  in
  let artifact =
    Riot_store.Artifact.{
      input_hash = Crypto.hash_string "test-input";
      output_hash = Crypto.hash_string "test-output";
      size_bytes = 0L;
      files = [];
      ocamlc_warnings = [];
      exports = [];
    }
  in
  let result_cached =
    Package_builder.{
      unit_key = unit_key package;
      package;
      status = Cached artifact;
      depset = [];
      action_artifacts = [];
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 5;
    }
  in
  let result_built =
    Package_builder.{
      unit_key = unit_key package;
      package;
      status = Built artifact;
      depset = [];
      action_artifacts = [];
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 100;
    }
  in
  let result_failed =
    Package_builder.{
      unit_key = unit_key package;
      package;
      status = Failed (ExecutionFailed { message = "compilation error" });
      depset = [];
      action_artifacts = [];
      ocamlc_warnings = [];
      duration = Time.Duration.from_millis 50;
    }
  in
  match (result_cached.status, result_built.status, result_failed.status) with
  | (Cached _, Built _, Failed _) -> Ok ()
  | _ -> Error "Status variants don't match expected types"

let test_package_error_variants = fun _ctx ->
  let planning_error = Riot_planner.Planning_error.Exception { exn = Failure "test" } in
  let error1 = Package_builder.PlanningFailed planning_error in
  let error2 = Package_builder.ExecutionFailed { message = "build failed" } in
  match (error1, error2) with
  | (PlanningFailed _, ExecutionFailed _) -> Ok ()
  | _ -> Error "Error variants don't match expected types"

let test_build_writes_hash_manifest_with_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"pkg_builder_export_manifest_test"
    (fun tmpdir ->
      let package_dir = Path.(tmpdir / Path.v "pkg") in
      let src_dir = Path.(package_dir / Path.v "src") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let _ =
        Fs.write "let answer = 42\n" Path.(src_dir / Path.v "lib.ml")
        |> Result.expect ~msg:"write source failed"
      in
      let package =
        Riot_model.Package.make
          ~name:(package_name "pkg")
          ~path:package_dir
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/lib.ml" }
          ~sources:{
            src = [ Path.v "src/lib.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace =
        Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ package ]
          ~target_dir:(Path.v "target")
          ()
      in
      let store = Riot_store.Store.create ~workspace in
      match build_workspace_package ~workspace package with
      | Error err -> Error ("build failed: " ^ err)
      | Ok package_result -> (
          match Riot_build.Build_result.package_status package_result with
          | Failed reason -> Error ("build failed: " ^ reason)
          | Skipped reason -> Error ("build skipped: " ^ reason)
          | Built artifact
          | Cached artifact -> (
              match Riot_store.Store.load_manifest store ~hash:artifact.input_hash with
              | None -> Error "expected package hash manifest to be saved"
              | Some manifest ->
                  if List.length manifest.Riot_store.Manifest.exports > 0 then
                    Ok ()
                  else
                    Error "expected hash manifest to include exported outputs"
            )
        )) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let make_library_package = fun ~root ~name ?(dependencies = []) source ->
  let package_dir = Path.(root / Path.v name) in
  let src_dir = Path.(package_dir / Path.v "src") in
  let source_path = Path.(src_dir / Path.v (name ^ ".ml")) in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"create src dir failed"
  in
  let _ =
    Fs.write source source_path
    |> Result.expect ~msg:"write source failed"
  in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:package_dir
    ~relative_path:(Path.v name)
    ~dependencies:(List.map dependencies ~fn:workspace_dependency)
    ~library:{ path = Path.v ("src/" ^ name ^ ".ml") }
    ~sources:{
      src = [ Path.v ("src/" ^ name ^ ".ml") ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let test_dependency_source_change_rebuilds_dependent_package = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"pkg_builder_dep_rebuild"
    (fun tmpdir ->
      let dep = make_library_package ~root:tmpdir ~name:"dep" "let value = 1\n" in
      let app =
        make_library_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ "dep" ]
          "let value = Dep.value\n"
      in
      let workspace =
        Riot_model.Workspace.make_realized
          ~root:tmpdir
          ~packages:[ dep; app ]
          ~target_dir:(Path.v "target")
          ()
      in
      let first_app = build_workspace_package ~workspace app in
      let dep_source = Path.(dep.path / Path.v "src" / Path.v "dep.ml") in
      let _ =
        Fs.write "let value = 2\n" dep_source
        |> Result.expect ~msg:"rewrite dep source failed"
      in
      let second_app = build_workspace_package ~workspace app in
      let artifact = fun result ->
        match result with
        | Error _ as err -> err
        | Ok package_result -> (
            match Riot_build.Build_result.package_artifact package_result with
            | Some artifact -> Ok artifact
            | None -> Error "expected app build result to carry an artifact"
          )
      in
      match (artifact first_app, artifact second_app) with
      | (Ok first_app_artifact, Ok second_app_artifact) ->
          if Crypto.Hash.equal first_app_artifact.input_hash second_app_artifact.input_hash then
            Error "expected dependent package artifact hash to change after dependency source edit"
          else
            Ok ()
      | (Error err, _) -> Error ("initial dependent build failed: " ^ err)
      | (_, Error err) -> Error ("dependent rebuild failed: " ^ err)) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.[
    case "collect_source_files: filters by extension" test_collect_source_files;
    case "build_result: status variants" test_build_result_status_variants;
    case "package_error: variants" test_package_error_variants;
    case "build writes hash manifest with exports" test_build_writes_hash_manifest_with_exports;
    case
      ~size:Large
      "dependency source change rebuilds dependent package"
      test_dependency_source_change_rebuilds_dependent_package;
  ]

let name = "Package Builder Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
