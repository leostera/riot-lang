open Std

module Test = Std.Test

let namespace = fun parts ->
  Contentstore.Namespace.from_parts parts
  |> Result.expect ~msg:"invalid test namespace"

let make_store = fun root parts ->
  Contentstore.create
    ~root
    ~ns:(namespace parts)
    ~policy:Contentstore.Policy.default

let with_root = fun prefix fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let root = Path.(tmpdir / Path.v "cache") in
      fn ~tmpdir ~root)
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let dir_entries = fun path ->
  match Fs.read_dir path with
  | Ok entries -> Iter.MutIterator.to_list entries
  | Error _ -> []

let test_create_on_existing_empty_root = fun _ctx ->
  with_root "contentstore-create-empty-root"
    (fun ~tmpdir:_ ~root ->
      let _ = Fs.create_dir_all root |> Result.expect ~msg:"create root should succeed" in
      let _store = make_store root [ "modules" ] in
      let exists = Fs.exists root |> Result.expect ~msg:"exists should succeed" in
      let entries = dir_entries root in
      if exists && List.is_empty entries then
        Ok ()
      else
        Error "expected create on an existing empty root to leave it unchanged")

let test_create_on_missing_root_is_lazy = fun _ctx ->
  with_root "contentstore-create-missing-root"
    (fun ~tmpdir:_ ~root ->
      let _store = make_store root [ "modules" ] in
      let exists = Fs.exists root |> Result.expect ~msg:"exists should succeed" in
      if exists then
        Error "expected create on a missing root to stay lazy until the first write"
      else
        Ok ())

let test_create_on_existing_populated_root = fun _ctx ->
  with_root "contentstore-create-populated-root"
    (fun ~tmpdir:_ ~root ->
      let marker = Path.(root / Path.v "unrelated.txt") in
      let _ = Fs.create_dir_all root |> Result.expect ~msg:"create root should succeed" in
      let _ = Fs.write "keep me" marker |> Result.expect ~msg:"write marker should succeed" in
      let _store = make_store root [ "modules" ] in
      match Fs.read_to_string marker with
      | Ok "keep me" -> Ok ()
      | Ok _ -> Error "expected create to preserve unrelated contents in the root"
      | Error _ -> Error "expected unrelated contents to stay readable after create")

let test_root_returns_configured_path = fun _ctx ->
  with_root "contentstore-root-path"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "modules" ] in
      if Path.equal (Contentstore.root store) root then
        Ok ()
      else
        Error "expected root to return the configured store root")

let test_hash_dir_is_stable_for_same_hash = fun _ctx ->
  with_root "contentstore-hash-dir-stable"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "modules" ] in
      let hash = Crypto.hash_string "same-hash" in
      if Path.equal (Contentstore.hash_dir_of store hash) (Contentstore.hash_dir_of store hash) then
        Ok ()
      else
        Error "expected hash_dir_of to be stable for the same hash")

let test_hash_dir_differs_for_distinct_hashes = fun _ctx ->
  with_root "contentstore-hash-dir-distinct"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "modules" ] in
      let left = Crypto.hash_string "left" in
      let right = Crypto.hash_string "right" in
      if Path.equal (Contentstore.hash_dir_of store left) (Contentstore.hash_dir_of store right) then
        Error "expected different hashes to map to different tree paths"
      else
        Ok ())

let test_exists_is_false_for_missing_hash = fun _ctx ->
  with_root "contentstore-exists-missing"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "modules" ] in
      if Contentstore.exists store (Crypto.hash_string "missing") then
        Error "expected exists to report false for missing hashes"
      else
        Ok ())

let test_exists_is_true_after_commit_dir = fun _ctx ->
  with_root "contentstore-exists-commit-dir"
    (fun ~tmpdir ~root ->
      let store = make_store root [ "modules" ] in
      let hash = Crypto.hash_string "committed-tree" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Fs.write "payload" Path.(source_dir / Path.v "payload.txt")
      |> Result.expect ~msg:"write payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir
      |> Result.expect ~msg:"commit_dir should succeed" in
      if Contentstore.exists store hash then
        Ok ()
      else
        Error "expected exists to report true after commit_dir")

let test_first_write_creates_missing_root = fun _ctx ->
  with_root "contentstore-create-first-write"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "modules" ] in
      let hash = Crypto.hash_string "first-write" in
      let hex = Crypto.Digest.hex hash in
      let _ = Contentstore.save_object store ~hash ~content:"payload"
      |> Result.expect ~msg:"save_object should succeed" in
      let exists = Fs.exists root |> Result.expect ~msg:"exists should succeed" in
      if not exists then
        Error "expected the first write to materialize the missing root"
      else
        match Fs.read_to_string Path.(root / Path.v "objects" / Path.v "modules" / Path.v (String.sub hex ~offset:0 ~len:2) / Path.v hex) with
        | Ok "payload" -> Ok ()
        | Ok _ -> Error "expected the first write to create a readable object under the root"
        | Error _ -> Error "expected the first write to materialize the saved object")

let test_reserved_like_namespace_parts_are_isolated = fun _ctx ->
  with_root "contentstore-create-reserved-ns"
    (fun ~tmpdir:_ ~root ->
      let store = make_store root [ "objects"; "__named"; "ab" ] in
      let hash = Crypto.hash_string "reserved-like" in
      let _ = Contentstore.save_object store ~hash ~content:"payload"
      |> Result.expect ~msg:"save_object should succeed" in
      let hex = Crypto.Digest.hex hash in
      let object_path =
        Path.(
          root
          / Path.v "objects"
          / Path.v "objects"
          / Path.v "__named"
          / Path.v "ab"
          / Path.v (String.sub hex ~offset:0 ~len:2)
          / Path.v hex
        )
      in
      let tree_path = Contentstore.hash_dir_of store hash in
      let object_exists = Fs.exists object_path |> Result.expect ~msg:"exists should succeed" in
      let tree_exists = Fs.exists tree_path |> Result.expect ~msg:"exists should succeed" in
      if object_exists && not tree_exists then
        Ok ()
      else
        Error "expected reserved-like namespace parts to stay isolated under objects/")

let tests = [
  Test.case "create on existing empty root leaves it unchanged" test_create_on_existing_empty_root;
  Test.case "create on missing root stays lazy" test_create_on_missing_root_is_lazy;
  Test.case "create on existing populated root preserves contents" test_create_on_existing_populated_root;
  Test.case "root returns configured path" test_root_returns_configured_path;
  Test.case "hash_dir_of is stable for the same hash" test_hash_dir_is_stable_for_same_hash;
  Test.case "hash_dir_of differs for different hashes" test_hash_dir_differs_for_distinct_hashes;
  Test.case "exists returns false for a missing hash" test_exists_is_false_for_missing_hash;
  Test.case "exists returns true after commit_dir" test_exists_is_true_after_commit_dir;
  Test.case "the first write creates a missing root" test_first_write_creates_missing_root;
  Test.case "reserved-like namespace parts are isolated safely" test_reserved_like_namespace_parts_are_isolated;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_creation_tests" ~tests ~args)
    ~args:Env.args
    ()
