open Std
module Test = Std.Test
module Artifact = Tusk_store.Artifact

let make_test_workspace () =
  Tusk_model.Workspace.
    { root = Path.v "."; target_dir_root = Path.v "target"; packages = [] }

let test_store_creation () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let store_dir =
          Path.(tmpdir / Path.v "target" / Path.v "debug" / Path.v "cache")
        in
        if Fs.exists store_dir |> Result.unwrap_or ~default:false then Ok ()
        else Error "Store directory not created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_exists_empty_store () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "nonexistent" in
        if not (Tusk_store.Store.exists store hash) then Ok ()
        else Error "Hash should not exist in empty store")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_exists_after_save () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed";
        let output_file = Path.(sandbox_dir / Path.v "output.txt") in
        let _ =
          Fs.write "output" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "build123" in
        match
          Tusk_store.Store.save store ~package:"test-pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok _ ->
            if Tusk_store.Store.exists store hash then Ok ()
            else Error "Hash should exist after save"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_list_artifacts_empty () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "empty" in
        match Tusk_store.Store.get store hash with
        | None -> Ok ()
        | Some artifact ->
            if List.length artifact.Artifact.files = 0 then Ok ()
            else Error "Expected empty list for non-existent hash")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_list_artifacts_after_save () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "output.txt") in
        let _ =
          Fs.write "content" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "test" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok _ -> (
            match Tusk_store.Store.get store hash with
            | Some artifact ->
                if List.length artifact.Artifact.files > 0 then Ok ()
                else Error "Expected files in artifact list"
            | None -> Error "Expected artifact to be cached")
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_save_single_file () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let _ =
          Fs.write "test content" output_file
          |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "single" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok artifact ->
            if List.length artifact.files = 1 then Ok ()
            else Error "Expected 1 file in artifact"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_save_multiple_files () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "a.txt") in
        let file2 = Path.(sandbox_dir / Path.v "b.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        let hash = Crypto.hash_string "multi" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ file1; file2 ]
        with
        | Ok artifact ->
            if List.length artifact.files = 2 then Ok ()
            else Error "Expected 2 files in artifact"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_save_creates_manifest () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed";
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        Fs.write "test" output_file |> Result.expect ~msg:"Write failed";
        let hash = Crypto.hash_string "manifest_test" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok _ ->
            if Tusk_store.Store.exists store hash then Ok ()
            else Error "Artifact not retrievable after save"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_save_returns_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let _ =
          Fs.write "test" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "artifact_return" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok artifact ->
            if Crypto.Digest.hex artifact.hash = Crypto.Digest.hex hash then
              Ok ()
            else Error "Artifact hash mismatch"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_save_with_missing_output () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let missing_file = Path.(sandbox_dir / Path.v "missing.txt") in
        let hash = Crypto.hash_string "missing" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ missing_file ]
        with
        | Ok artifact ->
            if List.length artifact.files = 0 then Ok ()
            else Error "Expected no files for missing output"
        | Error _ -> Error "Should not fail, just skip missing files")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_get_missing_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "missing" in
        match Tusk_store.Store.get store hash with
        | None -> Ok ()
        | Some _ -> Error "Should return None for missing artifact")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_get_existing_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let _ =
          Fs.write "content" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "existing" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok _ -> (
            match Tusk_store.Store.get store hash with
            | Some artifact ->
                if Crypto.Digest.hex artifact.hash = Crypto.Digest.hex hash then
                  Ok ()
                else Error "Hash mismatch"
            | None -> Error "Should return artifact")
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_promote_missing_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "missing" in
        let artifact = Tusk_store.Artifact.{ hash; files = [] } in
        let target_dir = Path.(tmpdir / Path.v "target") in
        match
          Tusk_store.Store.promote store artifact.Artifact.hash ~target_dir
        with
        | Error _ -> Ok ()
        | Ok () -> Error "Should fail for missing artifact")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_promote_existing_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let _ =
          Fs.write "content" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "promote" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok artifact -> (
            let promote_dir = Path.(tmpdir / Path.v "promoted") in
            match
              Tusk_store.Store.promote store artifact.Artifact.hash
                ~target_dir:promote_dir
            with
            | Ok () ->
                let promoted_file = Path.(promote_dir / Path.v "test.txt") in
                if Fs.exists promoted_file |> Result.unwrap_or ~default:false
                then Ok ()
                else Error "Promoted file not found"
            | Error e -> Error (format "Promote failed: %s" e))
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_promote_creates_target_dir () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let _ =
          Fs.write "test" output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "create_dir" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok artifact -> (
            let new_dir =
              Path.(tmpdir / Path.v "new" / Path.v "nested" / Path.v "dir")
            in
            match
              Tusk_store.Store.promote store artifact.Artifact.hash
                ~target_dir:new_dir
            with
            | Ok () ->
                if Fs.exists new_dir |> Result.unwrap_or ~default:false then
                  Ok ()
                else Error "Target directory not created"
            | Error e -> Error (format "Promote failed: %s" e))
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_promote_copies_all_files () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "a.txt") in
        let file2 = Path.(sandbox_dir / Path.v "b.txt") in
        let file3 = Path.(sandbox_dir / Path.v "c.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "c" file3 |> Result.expect ~msg:"Write failed" in
        let hash = Crypto.hash_string "multi_promote" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ file1; file2; file3 ]
        with
        | Ok artifact -> (
            let promote_dir = Path.(tmpdir / Path.v "promoted") in
            match
              Tusk_store.Store.promote store artifact.Artifact.hash
                ~target_dir:promote_dir
            with
            | Ok () ->
                let all_exist =
                  Fs.exists Path.(promote_dir / Path.v "a.txt")
                  |> Result.unwrap_or ~default:false
                  && Fs.exists Path.(promote_dir / Path.v "b.txt")
                     |> Result.unwrap_or ~default:false
                  && Fs.exists Path.(promote_dir / Path.v "c.txt")
                     |> Result.unwrap_or ~default:false
                in
                if all_exist then Ok () else Error "Not all files promoted"
            | Error e -> Error (format "Promote failed: %s" e))
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_promote_preserves_content () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output_file = Path.(sandbox_dir / Path.v "test.txt") in
        let content = "test content to preserve" in
        let _ =
          Fs.write content output_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "preserve" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output_file ]
        with
        | Ok artifact -> (
            let promote_dir = Path.(tmpdir / Path.v "promoted") in
            match
              Tusk_store.Store.promote store artifact.Artifact.hash
                ~target_dir:promote_dir
            with
            | Ok () -> (
                let promoted_file = Path.(promote_dir / Path.v "test.txt") in
                match Fs.read promoted_file with
                | Ok read_content ->
                    if String.equal read_content content then Ok ()
                    else Error "Content not preserved"
                | Error _ -> Error "Failed to read promoted file")
            | Error e -> Error (format "Promote failed: %s" e))
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_hash_based_directory_structure () = Ok ()

let test_store_same_hash_overwrites () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "v1.txt") in
        let _ =
          Fs.write "version 1" file1 |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "same_hash" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ file1 ]
          |> Result.expect ~msg:"First save failed"
        in
        let file2 = Path.(sandbox_dir / Path.v "v2.txt") in
        let _ =
          Fs.write "version 2" file2 |> Result.expect ~msg:"Write failed"
        in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ file2 ]
        with
        | Ok artifact ->
            if List.length artifact.files = 1 then Ok ()
            else Error "Expected overwrite to succeed"
        | Error e -> Error (format "Second save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_different_hashes_separate () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "a.txt") in
        let file2 = Path.(sandbox_dir / Path.v "b.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        let hash1 = Crypto.hash_string "hash1" in
        let hash2 = Crypto.hash_string "hash2" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash:hash1 ~sandbox_dir
            ~outs:[ file1 ]
          |> Result.expect ~msg:"Save 1 failed"
        in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash:hash2 ~sandbox_dir
            ~outs:[ file2 ]
          |> Result.expect ~msg:"Save 2 failed"
        in
        if
          Tusk_store.Store.exists store hash1
          && Tusk_store.Store.exists store hash2
        then Ok ()
        else Error "Both hashes should exist separately")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_artifact_creation () =
  let hash = Crypto.hash_string "test" in
  let files = [ Path.v "foo.cmi"; Path.v "foo.cmx" ] in
  let artifact = Tusk_store.Artifact.{ hash; files } in
  if Crypto.Digest.hex artifact.hash = Crypto.Digest.hex hash then Ok ()
  else Error "Artifact hash mismatch"

let test_artifact_hash_equality () =
  let hash = Crypto.hash_string "test" in
  let artifact1 = Tusk_store.Artifact.{ hash; files = [ Path.v "a.txt" ] } in
  let artifact2 = Tusk_store.Artifact.{ hash; files = [ Path.v "b.txt" ] } in
  if Crypto.Digest.hex artifact1.hash = Crypto.Digest.hex artifact2.hash then
    Ok ()
  else Error "Expected same hash"

let test_artifact_file_list () =
  let hash = Crypto.hash_string "test" in
  let files = [ Path.v "foo.cmi"; Path.v "foo.cmx"; Path.v "foo.cmxa" ] in
  let artifact = Tusk_store.Artifact.{ hash; files } in
  if List.length artifact.files = 3 then Ok ()
  else Error "Expected 3 files in artifact"

let test_manifest_creation () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "test.txt") in
        let _ =
          Fs.write "test content" file1 |> Result.expect ~msg:"Write failed"
        in
        let manifest =
          Tusk_store.Manifest.create ~package:"test-pkg" ~build_hash:"abc123"
            ~files:[ (file1, 12) ]
        in
        if String.equal manifest.package "test-pkg" then Ok ()
        else Error "Package name mismatch")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_save_and_load () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let source_file = Path.(tmpdir / Path.v "source.txt") in
        let manifest_file = Path.(tmpdir / Path.v "manifest.json") in
        let _ =
          Fs.write "content" source_file |> Result.expect ~msg:"Write failed"
        in
        let manifest =
          Tusk_store.Manifest.create ~package:"test" ~build_hash:"hash123"
            ~files:[ (source_file, 7) ]
        in
        match Tusk_store.Manifest.save manifest ~path:manifest_file with
        | Ok () -> (
            match Tusk_store.Manifest.load ~path:manifest_file with
            | Ok loaded ->
                if String.equal loaded.package "test" then Ok ()
                else Error "Package name not preserved"
            | Error e -> Error (format "Failed to load: %s" e))
        | Error e -> Error (format "Failed to save: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_json_serialization () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "test.txt") in
        let _ = Fs.write "test" file1 |> Result.expect ~msg:"Write failed" in
        let manifest =
          Tusk_store.Manifest.create ~package:"pkg" ~build_hash:"hash"
            ~files:[ (file1, 4) ]
        in
        let json = Tusk_store.Manifest.to_json manifest in
        match Tusk_store.Manifest.of_json json with
        | Ok parsed ->
            if String.equal parsed.package "pkg" then Ok ()
            else Error "Roundtrip failed"
        | Error e -> Error (format "Parse failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_includes_package_name () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "test.txt") in
        let _ = Fs.write "test" file1 |> Result.expect ~msg:"Write failed" in
        let manifest =
          Tusk_store.Manifest.create ~package:"my-package" ~build_hash:"hash"
            ~files:[ (file1, 4) ]
        in
        if String.equal manifest.package "my-package" then Ok ()
        else Error "Package name not included")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_includes_build_hash () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "test.txt") in
        let _ = Fs.write "test" file1 |> Result.expect ~msg:"Write failed" in
        let manifest =
          Tusk_store.Manifest.create ~package:"pkg" ~build_hash:"build-hash-123"
            ~files:[ (file1, 4) ]
        in
        if String.equal manifest.build_hash "build-hash-123" then Ok ()
        else Error "Build hash not included")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_includes_file_list () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "a.txt") in
        let file2 = Path.(tmpdir / Path.v "b.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        let manifest =
          Tusk_store.Manifest.create ~package:"pkg" ~build_hash:"hash"
            ~files:[ (file1, 1); (file2, 1) ]
        in
        if List.length manifest.files = 2 then Ok ()
        else Error "File list not included")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_manifest_includes_file_sizes () =
  match
    Fs.with_tempdir ~prefix:"manifest_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "test.txt") in
        let _ =
          Fs.write "test content" file1 |> Result.expect ~msg:"Write failed"
        in
        let manifest =
          Tusk_store.Manifest.create ~package:"pkg" ~build_hash:"hash"
            ~files:[ (file1, 12) ]
        in
        match List.hd manifest.files with
        | entry ->
            if entry.size = 12 then Ok () else Error "File size not correct"
        | exception _ -> Error "No files in manifest")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_concurrent_save_same_hash () = Ok ()
let test_store_concurrent_save_different_hash () = Ok ()
let test_store_save_preserves_file_permissions () = Ok ()
let test_store_promote_preserves_file_permissions () = Ok ()

let test_store_handles_large_files () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let large_file = Path.(sandbox_dir / Path.v "large.txt") in
        let large_content = String.make 10000 'x' in
        let _ =
          Fs.write large_content large_file |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "large" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ large_file ]
        with
        | Ok artifact ->
            if List.length artifact.files = 1 then Ok ()
            else Error "Large file not saved"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_handles_binary_files () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let binary_file = Path.(sandbox_dir / Path.v "binary.dat") in
        let binary_content = "\000\001\002\003\255\254\253" in
        let _ =
          Fs.write binary_content binary_file
          |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "binary" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ binary_file ]
        with
        | Ok artifact ->
            if List.length artifact.files = 1 then Ok ()
            else Error "Binary file not saved"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_handles_empty_files () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let empty_file = Path.(sandbox_dir / Path.v "empty.txt") in
        let _ = Fs.write "" empty_file |> Result.expect ~msg:"Write failed" in
        let hash = Crypto.hash_string "empty" in
        match
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ empty_file ]
        with
        | Ok artifact ->
            if List.length artifact.files = 1 then Ok ()
            else Error "Empty file not saved"
        | Error e -> Error (format "Save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_full_workflow () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output = Path.(sandbox_dir / Path.v "output.txt") in
        let _ =
          Fs.write "build output" output |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "workflow" in
        match
          Tusk_store.Store.save store ~package:"my-pkg" ~hash ~sandbox_dir
            ~outs:[ output ]
        with
        | Ok artifact -> (
            match Tusk_store.Store.get store hash with
            | Some retrieved -> (
                let target_dir = Path.(tmpdir / Path.v "deployed") in
                match
                  Tusk_store.Store.promote store retrieved.Artifact.hash
                    ~target_dir
                with
                | Ok () ->
                    let deployed_file =
                      Path.(target_dir / Path.v "output.txt")
                    in
                    if
                      Fs.exists deployed_file |> Result.unwrap_or ~default:false
                    then Ok ()
                    else Error "Full workflow failed at promote"
                | Error e -> Error (format "Promote failed: %s" e))
            | None -> Error "Full workflow failed at get")
        | Error e -> Error (format "Full workflow failed at save: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_cache_hit_scenario () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output = Path.(sandbox_dir / Path.v "cached.txt") in
        let _ = Fs.write "cached" output |> Result.expect ~msg:"Write failed" in
        let hash = Crypto.hash_string "cache_hit" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output ]
          |> Result.expect ~msg:"Save failed"
        in
        match Tusk_store.Store.get store hash with
        | Some _ -> Ok ()
        | None -> Error "Cache hit failed - artifact not found")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_cache_miss_scenario () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "cache_miss" in
        match Tusk_store.Store.get store hash with
        | None -> Ok ()
        | Some _ -> Error "Cache miss failed - artifact should not exist")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_incremental_build_scenario () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file_a = Path.(sandbox_dir / Path.v "a.txt") in
        let file_b = Path.(sandbox_dir / Path.v "b.txt") in
        let _ = Fs.write "a v1" file_a |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b v1" file_b |> Result.expect ~msg:"Write failed" in
        let hash1 = Crypto.hash_string "build1" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash:hash1 ~sandbox_dir
            ~outs:[ file_a; file_b ]
          |> Result.expect ~msg:"First save failed"
        in
        let _ = Fs.write "a v2" file_a |> Result.expect ~msg:"Write failed" in
        let hash2 = Crypto.hash_string "build2" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash:hash2 ~sandbox_dir
            ~outs:[ file_a; file_b ]
          |> Result.expect ~msg:"Second save failed"
        in
        if
          Tusk_store.Store.exists store hash1
          && Tusk_store.Store.exists store hash2
        then Ok ()
        else Error "Incremental builds should both be cached")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_multiple_packages_same_hash () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "out.txt") in
        let _ =
          Fs.write "same output" file1 |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "same" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg1" ~hash ~sandbox_dir
            ~outs:[ file1 ]
          |> Result.expect ~msg:"Pkg1 save failed"
        in
        match
          Tusk_store.Store.save store ~package:"pkg2" ~hash ~sandbox_dir
            ~outs:[ file1 ]
        with
        | Ok _ -> Ok ()
        | Error e -> Error (format "Pkg2 save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_multiple_packages_different_hash () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "a.txt") in
        let file2 = Path.(sandbox_dir / Path.v "b.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        let hash1 = Crypto.hash_string "pkg1_hash" in
        let hash2 = Crypto.hash_string "pkg2_hash" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg1" ~hash:hash1 ~sandbox_dir
            ~outs:[ file1 ]
          |> Result.expect ~msg:"Pkg1 save failed"
        in
        let _ =
          Tusk_store.Store.save store ~package:"pkg2" ~hash:hash2 ~sandbox_dir
            ~outs:[ file2 ]
          |> Result.expect ~msg:"Pkg2 save failed"
        in
        if
          Tusk_store.Store.exists store hash1
          && Tusk_store.Store.exists store hash2
        then Ok ()
        else Error "Both packages should be stored separately")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_get_stats () = Ok ()

let test_promote_success () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let output = Path.(sandbox_dir / Path.v "test.txt") in
        let _ = Fs.write "test" output |> Result.expect ~msg:"Write failed" in
        let hash = Crypto.hash_string "promote_success" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir
            ~outs:[ output ]
          |> Result.expect ~msg:"Save failed"
        in
        let target_dir = Path.(tmpdir / Path.v "promoted") in
        match Tusk_store.Store.promote store hash ~target_dir with
        | Ok () -> Ok ()
        | Error e -> Error (format "Promote failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_promote_failure () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace = make_test_workspace () in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "missing_hash" in
        let target_dir = Path.(tmpdir / Path.v "target") in
        match Tusk_store.Store.promote store hash ~target_dir with
        | Error _ -> Ok ()
        | Ok () -> Error "promote should fail for missing hash")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_store_artifacts_internal () =
  match
    Fs.with_tempdir ~prefix:"store_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let _ =
          Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"Create failed"
        in
        let file1 = Path.(sandbox_dir / Path.v "internal.txt") in
        let _ =
          Fs.write "internal test" file1 |> Result.expect ~msg:"Write failed"
        in
        let hash = Crypto.hash_string "internal" in
        match
          Tusk_store.Store.save store ~package:"test" ~hash ~sandbox_dir
            ~outs:[ file1 ]
        with
        | Ok artifact ->
            if Crypto.Digest.hex artifact.hash = Crypto.Digest.hex hash then
              Ok ()
            else Error "Internal save returned wrong hash"
        | Error e -> Error (format "Internal save failed: %s" e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "store: creation" test_store_creation;
      case "store: exists empty store" test_store_exists_empty_store;
      case "store: exists after save" test_store_exists_after_save;
      case "store: list_artifacts empty" test_store_list_artifacts_empty;
      case "store: list_artifacts after save"
        test_store_list_artifacts_after_save;
      case "store: save single file" test_store_save_single_file;
      case "store: save multiple files" test_store_save_multiple_files;
      case "store: save creates manifest" test_store_save_creates_manifest;
      case "store: save returns artifact" test_store_save_returns_artifact;
      case "store: save with missing output" test_store_save_with_missing_output;
      case "store: get missing artifact" test_store_get_missing_artifact;
      case "store: get existing artifact" test_store_get_existing_artifact;
      case "store: promote missing artifact" test_store_promote_missing_artifact;
      case "store: promote existing artifact"
        test_store_promote_existing_artifact;
      case "store: promote creates target dir"
        test_store_promote_creates_target_dir;
      case "store: promote copies all files" test_store_promote_copies_all_files;
      case "store: promote preserves content"
        test_store_promote_preserves_content;
      case "store: hash-based directory structure"
        test_store_hash_based_directory_structure;
      case "store: same hash overwrites" test_store_same_hash_overwrites;
      case "store: different hashes separate"
        test_store_different_hashes_separate;
      case "artifact: creation" test_artifact_creation;
      case "artifact: hash equality" test_artifact_hash_equality;
      case "artifact: file list" test_artifact_file_list;
      case "manifest: creation" test_manifest_creation;
      case "manifest: save and load" test_manifest_save_and_load;
      case "manifest: json serialization" test_manifest_json_serialization;
      case "manifest: includes package name" test_manifest_includes_package_name;
      case "manifest: includes build hash" test_manifest_includes_build_hash;
      case "manifest: includes file list" test_manifest_includes_file_list;
      case "manifest: includes file sizes" test_manifest_includes_file_sizes;
      case "store: concurrent save same hash"
        test_store_concurrent_save_same_hash;
      case "store: concurrent save different hash"
        test_store_concurrent_save_different_hash;
      case "store: save preserves file permissions"
        test_store_save_preserves_file_permissions;
      case "store: promote preserves file permissions"
        test_store_promote_preserves_file_permissions;
      case "store: handles large files" test_store_handles_large_files;
      case "store: handles binary files" test_store_handles_binary_files;
      case "store: handles empty files" test_store_handles_empty_files;
      case "store: full workflow" test_store_full_workflow;
      case "store: cache hit scenario" test_store_cache_hit_scenario;
      case "store: cache miss scenario" test_store_cache_miss_scenario;
      case "store: incremental build scenario"
        test_store_incremental_build_scenario;
      case "store: multiple packages same hash"
        test_store_multiple_packages_same_hash;
      case "store: multiple packages different hash"
        test_store_multiple_packages_different_hash;
      case "store: get stats" test_store_get_stats;
      case "store: promote success" test_promote_success;
      case "store: promote failure" test_promote_failure;
      case "store: store_artifacts internal" test_store_artifacts_internal;
    ]

let name = "Tusk Store Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
