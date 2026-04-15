open Std
module Test = Std.Test

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let make_store = fun tmpdir parts ->
  Contentstore.create
    ~root:Path.(tmpdir / Path.v "cache")
    ~ns:(namespace parts)
    ~policy:Contentstore.Policy.default

let with_store = fun prefix parts fn ->
  Fs.with_tempdir ~prefix (fun tmpdir -> fn ~tmpdir ~store:(make_store tmpdir parts))
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let write_tree = fun root entries ->
  let rec loop entries =
    match entries with
    | [] -> Ok ()
    | (relative_path, content) :: rest ->
        let path = Path.(root / relative_path) in
        let _ = Fs.create_dir_all (Path.dirname path) |> Result.expect ~msg:"create parent dirs should succeed" in
        let _ = Fs.write content path |> Result.expect ~msg:"write tree file should succeed" in
        loop rest
  in
  loop entries

let read_tree_file = fun root relative_path -> Fs.read_to_string Path.(root / relative_path)

let test_commit_dir_first_writer_wins = fun _ctx ->
  with_store "contentstore-commit-dir" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "same-tree" in
      let first_dir = Path.(tmpdir / Path.v "first") in
      let second_dir = Path.(tmpdir / Path.v "second") in
      let target_file = Path.v "payload.txt" in
      let _ = Fs.create_dir_all first_dir |> Result.expect ~msg:"create first dir should succeed" in
      let _ = Fs.create_dir_all second_dir |> Result.expect ~msg:"create second dir should succeed" in
      let _ = Fs.write "first" Path.(first_dir / target_file) |> Result.expect ~msg:"write first payload should succeed" in
      let _ = Fs.write "second" Path.(second_dir / target_file) |> Result.expect ~msg:"write second payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir:first_dir |> Result.expect ~msg:"first commit_dir should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir:second_dir |> Result.expect ~msg:"second commit_dir should succeed" in
      let committed = Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / target_file)
      |> Result.expect ~msg:"committed payload should exist" in
      if String.equal committed "first" then
        Ok ()
      else
        Error "expected first tree writer to win")

let test_commit_dir_with_one_file = fun _ctx ->
  with_store "contentstore-commit-dir-one-file" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "one-file" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Fs.write "payload" Path.(source_dir / Path.v "payload.txt") |> Result.expect ~msg:"write payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      match Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / Path.v "payload.txt") with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected one-file tree commit to stay readable"
      | Error _ -> Error "expected committed one-file tree to exist")

let test_commit_dir_preserves_nested_structure_and_bytes = fun _ctx ->
  with_store "contentstore-commit-dir-nested" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "nested-tree" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let entries = [
        (Path.(Path.v "a" / Path.v "payload.txt"), "alpha");
        (Path.(Path.v "a" / Path.v "b" / Path.v "binary.bin"), "bin\000\255");
        (Path.(Path.v "c" / Path.v "d" / Path.v "leaf.txt"), "omega");
      ] in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = write_tree source_dir entries |> Result.expect ~msg:"write tree should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      let committed_root = Contentstore.hash_dir_of store hash in
      match (
        read_tree_file committed_root Path.(Path.v "a" / Path.v "payload.txt"),
        read_tree_file committed_root Path.(Path.v "a" / Path.v "b" / Path.v "binary.bin"),
        read_tree_file committed_root Path.(Path.v "c" / Path.v "d" / Path.v "leaf.txt")
      ) with
      | (Ok "alpha", Ok "bin\000\255", Ok "omega") -> Ok ()
      | _ -> Error "expected commit_dir to preserve nested structure and file bytes exactly")

let test_commit_dir_of_empty_directory_creates_empty_destination = fun _ctx ->
  with_store "contentstore-commit-dir-empty" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "empty-tree" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      let destination = Contentstore.hash_dir_of store hash in
      let exists = Fs.exists destination |> Result.expect ~msg:"exists should succeed" in
      let entries =
        match Fs.read_dir destination with
        | Ok iter -> Iter.MutIterator.to_list iter
        | Error _ -> []
      in
      if exists && List.is_empty entries then
        Ok ()
      else
        Error "expected commit_dir of an empty directory to create an empty destination")

let test_commit_dir_large_tree_remains_readable = fun _ctx ->
  with_store "contentstore-commit-dir-large" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "large-tree" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let rec write_many index =
        if index = 100 then
          Ok ()
        else
          let relative = Path.(Path.v "files" / Path.v ("file-" ^ Int.to_string index ^ ".txt")) in
          let path = Path.(source_dir / relative) in
          let _ = Fs.create_dir_all (Path.dirname path) |> Result.expect ~msg:"create parent dirs should succeed" in
          let _ = Fs.write ("payload-" ^ Int.to_string index) path |> Result.expect ~msg:"write payload should succeed" in
          write_many (index + 1)
      in
      let rec verify_many root index =
        if index = 100 then
          Ok ()
        else
          let relative = Path.(Path.v "files" / Path.v ("file-" ^ Int.to_string index ^ ".txt")) in
          match Fs.read_to_string Path.(root / relative) with
          | Ok loaded when String.equal loaded ("payload-" ^ Int.to_string index) -> verify_many
            root
            (index + 1)
          | _ -> Error "expected all files in a large committed tree to remain readable"
      in
      let _ = write_many 0 |> Result.expect ~msg:"write large tree should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      verify_many (Contentstore.hash_dir_of store hash) 0)

let test_commit_dir_rejects_file_source = fun _ctx ->
  with_store "contentstore-commit-dir-invalid-source" [ "modules" ]
    (fun ~tmpdir ~store ->
      let source = Path.(tmpdir / Path.v "source.txt") in
      let _ = Fs.write "not a directory" source |> Result.expect ~msg:"write source should succeed" in
      match Contentstore.commit_dir store ~hash:(Crypto.hash_string "invalid") ~source_dir:source with
      | Error (Contentstore.Store.Invalid_source_path {
        reason=Contentstore.Store.Source_not_directory;
        _
      }) -> Ok ()
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok () -> Error "expected commit_dir to reject a file source")

let test_commit_dir_rejects_missing_source = fun _ctx ->
  with_store "contentstore-commit-dir-missing-source" [ "modules" ]
    (fun ~tmpdir ~store ->
      let source = Path.(tmpdir / Path.v "missing") in
      match Contentstore.commit_dir store ~hash:(Crypto.hash_string "missing") ~source_dir:source with
      | Error (Contentstore.Store.Invalid_source_path { reason=Contentstore.Store.Source_missing; _ }) -> Ok ()
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok () -> Error "expected commit_dir to reject a missing source path")

let test_commit_dir_preserves_empty_subdirectories = fun _ctx ->
  with_store "contentstore-commit-dir-empty-subdirs" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "empty-subdirs" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let empty_dir = Path.(source_dir / Path.v "a" / Path.v "b") in
      let _ = Fs.create_dir_all empty_dir |> Result.expect ~msg:"create nested empty dir should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      let exists = Fs.exists Path.(Contentstore.hash_dir_of store hash / Path.v "a" / Path.v "b")
      |> Result.expect ~msg:"exists should succeed" in
      if exists then
        Ok ()
      else
        Error "expected commit_dir to preserve empty subdirectories")

let test_commit_dir_consumes_source_dir_on_success = fun _ctx ->
  with_store "contentstore-commit-dir-consumes-source" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "consume-source" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Fs.write "payload" Path.(source_dir / Path.v "payload.txt") |> Result.expect ~msg:"write payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      let source_exists = Fs.exists source_dir |> Result.expect ~msg:"exists should succeed" in
      if source_exists then
        Error "expected successful commit_dir to consume the source directory"
      else
        Ok ())

let test_duplicate_commit_consumes_second_source_dir = fun _ctx ->
  with_store "contentstore-commit-dir-disposable-source" [ "modules" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "duplicate-source" in
      let first_dir = Path.(tmpdir / Path.v "first") in
      let second_dir = Path.(tmpdir / Path.v "second") in
      let _ = Fs.create_dir_all first_dir |> Result.expect ~msg:"create first dir should succeed" in
      let _ = Fs.create_dir_all second_dir |> Result.expect ~msg:"create second dir should succeed" in
      let _ = Fs.write "first" Path.(first_dir / Path.v "payload.txt") |> Result.expect ~msg:"write first payload should succeed" in
      let _ = Fs.write "second" Path.(second_dir / Path.v "payload.txt") |> Result.expect ~msg:"write second payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir:first_dir |> Result.expect ~msg:"first commit_dir should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir:second_dir |> Result.expect ~msg:"second commit_dir should succeed" in
      let second_exists = Fs.exists second_dir |> Result.expect ~msg:"exists should succeed" in
      match Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / Path.v "payload.txt") with
      | Ok "first" when not second_exists -> Ok ()
      | Ok _ -> Error "expected duplicate commit_dir to preserve the first tree contents"
      | Error _ -> Error "expected committed tree to stay readable after duplicate commit")

let test_different_hashes_create_distinct_tree_destinations = fun _ctx ->
  with_store "contentstore-commit-dir-distinct-hashes" [ "modules" ]
    (fun ~tmpdir ~store ->
      let left_hash = Crypto.hash_string "left-tree" in
      let right_hash = Crypto.hash_string "right-tree" in
      let left_dir = Path.(tmpdir / Path.v "left") in
      let right_dir = Path.(tmpdir / Path.v "right") in
      let _ = Fs.create_dir_all left_dir |> Result.expect ~msg:"create left dir should succeed" in
      let _ = Fs.create_dir_all right_dir |> Result.expect ~msg:"create right dir should succeed" in
      let _ = Fs.write "left" Path.(left_dir / Path.v "payload.txt") |> Result.expect ~msg:"write left payload should succeed" in
      let _ = Fs.write "right" Path.(right_dir / Path.v "payload.txt") |> Result.expect ~msg:"write right payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash:left_hash ~source_dir:left_dir
      |> Result.expect ~msg:"left commit_dir should succeed" in
      let _ = Contentstore.commit_dir store ~hash:right_hash ~source_dir:right_dir
      |> Result.expect ~msg:"right commit_dir should succeed" in
      match (
        Fs.read_to_string Path.(Contentstore.hash_dir_of store left_hash / Path.v "payload.txt"),
        Fs.read_to_string Path.(Contentstore.hash_dir_of store right_hash / Path.v "payload.txt")
      ) with
      | (Ok "left", Ok "right") -> Ok ()
      | _ -> Error "expected different hashes to create distinct readable tree destinations")

let test_commit_dir_source_inside_store_is_safe = fun _ctx ->
  with_store "contentstore-commit-dir-inside-store" [ "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "inside-store" in
      let source_dir = Path.(Contentstore.root store / Path.v "scratch" / Path.v "source") in
      let _ = Fs.create_dir_all source_dir |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Fs.write "payload" Path.(source_dir / Path.v "payload.txt") |> Result.expect ~msg:"write payload should succeed" in
      let _ = Contentstore.commit_dir store ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      match Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / Path.v "payload.txt") with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected commit_dir to stay consistent when the source dir lives under the store root"
      | Error _ -> Error "expected committed tree to stay readable when source_dir starts inside the store")

let tests = [
  Test.case "commit_dir keeps first writer" test_commit_dir_first_writer_wins;
  Test.case "commit_dir commits a one-file tree" test_commit_dir_with_one_file;
  Test.case "commit_dir preserves nested structure and bytes" test_commit_dir_preserves_nested_structure_and_bytes;
  Test.case "commit_dir on an empty directory creates an empty destination" test_commit_dir_of_empty_directory_creates_empty_destination;
  Test.case "commit_dir keeps a large tree readable" test_commit_dir_large_tree_remains_readable;
  Test.case "commit_dir rejects file source" test_commit_dir_rejects_file_source;
  Test.case "commit_dir rejects missing source" test_commit_dir_rejects_missing_source;
  Test.case "commit_dir preserves empty subdirectories" test_commit_dir_preserves_empty_subdirectories;
  Test.case "commit_dir consumes the source directory on success" test_commit_dir_consumes_source_dir_on_success;
  Test.case "duplicate commit_dir consumes the second source directory" test_duplicate_commit_consumes_second_source_dir;
  Test.case "different hashes create distinct tree destinations" test_different_hashes_create_distinct_tree_destinations;
  Test.case "commit_dir is safe when the source dir starts inside the store" test_commit_dir_source_inside_store_is_safe;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_tree_tests" ~tests ~args)
    ~args:Env.args
    ()
