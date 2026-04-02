open Std
module Test = Std.Test

let make_test_workspace = fun tmpdir ->
  Riot_model.Workspace.{
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let read_file = fun path -> Fs.read_to_string path |> Result.expect ~msg:"failed to read file"

let test_save_and_promote_nested_outputs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_nested_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "obj" / Path.v "x") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let nested_obj = Path.(sandbox / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let nested_bin = Path.(sandbox / Path.v "bin" / Path.v "app") in
        let _ = Fs.write "obj" nested_obj in
        let _ = Fs.write "bin" nested_bin in
        let hash = Crypto.hash_string "nested-artifact" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ nested_obj; nested_bin ]
        |> Result.expect ~msg:"save should succeed" in
        let target = Path.(tmpdir / Path.v "out") in
        let _ = Riot_store.Store.promote store hash ~target_dir:target |> Result.expect ~msg:"promote should succeed" in
        let promoted_obj = Path.(target / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let promoted_bin = Path.(target / Path.v "bin" / Path.v "app") in
        if
          String.equal (read_file promoted_obj) "obj" && String.equal (read_file promoted_bin) "bin"
        then
          Ok ()
        else
          Error "nested files were not promoted correctly")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_get_preserves_relative_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_manifest_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "deep" / Path.v "dir") in
        let output = Path.(sandbox / Path.v "deep" / Path.v "dir" / Path.v "x.cmxa") in
        let _ = Fs.write "cmxa" output in
        let hash = Crypto.hash_string "manifest-relative-paths" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.get store hash with
        | None -> Error "expected cached artifact"
        | Some artifact ->
            if List.mem (Path.v "deep/dir/x.cmxa") artifact.files then
              Ok ()
            else
              Error "artifact paths should keep nested relative paths")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_exists_requires_manifest_file = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_exists_manifest_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "hash-without-manifest" in
        let hash_dir = Riot_store.Store.hash_dir_of store hash in
        let _ = Fs.create_dir_all hash_dir in
        if Riot_store.Store.exists store hash then
          Error "exists should require manifest.json"
        else
          Ok ())
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_put_if_absent_keeps_first_writer = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_put_if_absent_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox in
        let output = Path.(sandbox / Path.v "x.txt") in
        let hash = Crypto.hash_string "same-hash" in
        let _ = Fs.write "first" output in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"first save should succeed" in
        let _ = Fs.write "second" output in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"second save should succeed" in
        let target = Path.(tmpdir / Path.v "out") in
        let _ = Riot_store.Store.promote store hash ~target_dir:target |> Result.expect ~msg:"promote should succeed" in
        if String.equal (read_file Path.(target / Path.v "x.txt")) "first" then
          Ok ()
        else
          Error "second writer should not replace existing cache entry")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_round_trip = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_plan_bundle_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "plan-bundle" in
        let plan = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String "pkg");
          ("action_graph", Std.Data.Json.Object [ ("nodes", Std.Data.Json.Array []) ]);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash ~plan |> Result.expect ~msg:"save_plan_bundle should succeed" in
        match Riot_store.Store.load_plan_bundle store ~hash with
        | None -> Error "expected saved plan bundle"
        | Some loaded ->
            if String.equal (Std.Data.Json.to_string loaded) (Std.Data.Json.to_string plan) then
              Ok ()
            else
              Error "loaded plan bundle should match saved bundle")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_hash_manifest_round_trip_preserves_exports = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_manifest_exports_round_trip_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "exports-hash" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "lib") in
        let output = Path.(sandbox / Path.v "lib" / Path.v "foo.cmxa") in
        let _ = Fs.write "cmxa" output in
        let exports = [
          Riot_store.Store.{
            name = "foo.cmxa";
            path = Path.v "lib/foo.cmxa";
            action_hash = Crypto.Digest.hex hash
          };
          Riot_store.Store.{
            name = "foo.cmxs";
            path = Path.v "lib/foo.cmxs";
            action_hash = Crypto.Digest.hex hash
          };
        ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.load_manifest store ~hash with
        | None -> Error "expected saved package hash manifest"
        | Some loaded ->
            let loaded = loaded.Riot_store.Manifest.exports in
            if List.length loaded = 2 then
              Ok ()
            else
              Error "expected two package export entries")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_artifact_round_trip_preserves_ocamlc_warnings = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_ocamlc_warnings_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox in
        let output = Path.(sandbox / Path.v "lib.cmx") in
        let _ = Fs.write "compiled" output in
        let hash = Crypto.hash_string "artifact-with-warnings" in
        let exports = [
          Riot_store.Store.{
            name = "lib.cmx";
            path = Path.v "lib.cmx";
            action_hash = Crypto.Digest.hex hash
          };
        ] in
        let warnings = [ "File \"lib.ml\", line 1, characters 0-1:\nWarning: example" ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~ocamlc_warnings:warnings
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.get store hash with
        | None -> Error "expected saved artifact"
        | Some artifact ->
            if artifact.ocamlc_warnings = warnings && artifact.exports = exports then
              Ok ()
            else
              Error "expected hash manifest payload to round-trip through the store")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_export_source_path_round_trip = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_export_source_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "find-export-hash" in
        let action_hash = Crypto.Digest.hex hash in
        let rel_path = Path.v "bin/tool" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") |> Result.expect ~msg:"create bin dir should succeed" in
        let _ = Fs.write "tool" Path.(sandbox / rel_path) |> Result.expect ~msg:"write export should succeed" in
        let exports = [ Riot_store.Store.{ name = "tool"; path = rel_path; action_hash } ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ Path.(sandbox / rel_path) ]
        |> Result.expect ~msg:"save should succeed" in
        let expected = Path.(Riot_store.Store.hash_dir_of store hash / rel_path) in
        match Riot_store.Store.export_source_path store (List.hd exports) with
        | None -> Error "expected to resolve package export path"
        | Some resolved ->
            if Path.to_string resolved = Path.to_string expected then
              Ok ()
            else
              Error "resolved export path did not match expected immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_export_source_path_rejects_absolute_export_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_export_source_path_absolute_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "absolute-export-hash" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let output = Path.(sandbox / Path.v "tool") in
        let _ = Fs.write "tool" output |> Result.expect ~msg:"write output should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "/tmp/not-allowed";
            action_hash = "deadbeef"
          };
        ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.export_source_path store (List.hd exports) with
        | None -> Ok ()
        | Some _ -> Error "absolute export paths should be rejected when resolving immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_load_manifest_returns_none_for_malformed_payload = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_load_manifest_malformed_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "malformed-manifest" in
        let hash_dir = Riot_store.Store.hash_dir_of store hash in
        let manifest_path = Path.(hash_dir / Path.v "manifest.json") in
        let _ = Fs.create_dir_all hash_dir |> Result.expect ~msg:"create hash dir should succeed" in
        let _ = Fs.write "{\"version\":\"v1\",\"exports\":\"not-an-array\"}" manifest_path
        |> Result.expect ~msg:"write malformed manifest should succeed" in
        match Riot_store.Store.load_manifest store ~hash with
        | None -> Ok ()
        | Some _ -> Error "expected malformed hash manifest to return None")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_materialize_package_exports_from_action_artifact = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_materialize_exports_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let output = Path.(sandbox / Path.v "bin" / Path.v "tool") in
        let _ = Fs.write "tool-binary" output in
        let action_hash = Crypto.hash_string "materialize-action" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash:action_hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save action artifact should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "bin/tool";
            action_hash = Crypto.Digest.hex action_hash
          };
        ] in
        let target_dir = Path.(tmpdir / Path.v "out" / Path.v "pkg") in
        let _ = Riot_store.Store.materialize_package_exports store ~exports ~target_dir
        |> Result.expect ~msg:"materialize_package_exports should succeed" in
        let materialized = Path.(target_dir / Path.v "tool") in
        if String.equal (read_file materialized) "tool-binary" then
          Ok ()
        else
          Error "materialized export content did not match source artifact")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_materialize_package_exports_fails_when_source_missing = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_materialize_exports_missing_source_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "bin/tool";
            action_hash = "missing-action-hash"
          };
        ] in
        let target_dir = Path.(tmpdir / Path.v "out" / Path.v "pkg") in
        match Riot_store.Store.materialize_package_exports store ~exports ~target_dir with
        | Ok () -> Error "expected missing export source to fail materialization"
        | Error message ->
            if String.contains message "cache is corrupted; try `riot clean`" then
              Ok ()
            else
              Error ("unexpected error: " ^ message))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "save/promote nested outputs" test_save_and_promote_nested_outputs;
    case "get preserves relative paths" test_get_preserves_relative_paths;
    case "exists requires manifest" test_exists_requires_manifest_file;
    case "put-if-absent keeps first writer" test_put_if_absent_keeps_first_writer;
    case "plan bundle round trip" test_plan_bundle_round_trip;
    case "hash manifest round trip preserves exports" test_hash_manifest_round_trip_preserves_exports;
    case "artifact round trip preserves manifest payload" test_artifact_round_trip_preserves_ocamlc_warnings;
    case "export source path round trip" test_export_source_path_round_trip;
    case "export source path rejects absolute paths" test_export_source_path_rejects_absolute_export_paths;
    case "load manifest returns none for malformed payload" test_load_manifest_returns_none_for_malformed_payload;
    case "materialize package exports from action artifacts" test_materialize_package_exports_from_action_artifact;
    case "materialize package exports fails when source missing" test_materialize_package_exports_fails_when_source_missing;
  ]

let name = "Riot Store Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
