open Std

module Test = Std.Test

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file |> Result.expect ~msg:"read_to_end should succeed" in
  let _ = Fs.File.close file |> Result.expect ~msg:"close should succeed" in
  content

let namespace = fun parts ->
  Contentstore.Namespace.from_parts parts
  |> Result.expect ~msg:"invalid test namespace"

let make_store = fun tmpdir parts ->
  Contentstore.create
    ~root:Path.(tmpdir / Path.v "cache")
    ~ns:(namespace parts)
    ~policy:Contentstore.Policy.default

let with_store = fun prefix parts fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir -> fn ~tmpdir ~store:(make_store tmpdir parts))
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let open_object_to_string = fun store ~hash ->
  Contentstore.open_object store ~hash
  |> Result.map ~fn:read_opened_file

let object_namespace_dir = fun store ->
  Path.(Contentstore.root store / Path.v "objects" / Path.v "typ" / Path.v "modules")

let test_save_object_roundtrip = fun _ctx ->
  with_store "contentstore-object-roundtrip" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "object-roundtrip" in
      let content = "hello\000world\255" in
      let _ = Contentstore.save_object store ~hash ~content |> Result.expect ~msg:"save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok loaded when String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected opened object to match saved content"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_empty_object_roundtrip = fun _ctx ->
  with_store "contentstore-object-empty" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "empty-object" in
      let _ = Contentstore.save_object store ~hash ~content:""
      |> Result.expect ~msg:"save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok "" -> Ok ()
      | Ok _ -> Error "expected opened empty object to stay empty"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_small_ascii_object_roundtrip = fun _ctx ->
  with_store "contentstore-object-ascii" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "ascii-object" in
      let content = "hello world" in
      let _ = Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok loaded when String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected opened ASCII object to match saved content"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_large_object_roundtrip = fun _ctx ->
  with_store "contentstore-object-large" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "large-object" in
      let content =
        String.init ~len:1_000_000 ~fn:(fun index ->
          Char.from_int_unchecked (Char.to_int 'a' + (index mod 26)))
      in
      let _ = Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"save_object should succeed" in
      match Contentstore.open_object store ~hash |> Result.map ~fn:read_opened_file with
      | Ok loaded when String.length loaded = String.length content && String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected opened large object to match saved content"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_object_first_writer_wins = fun _ctx ->
  with_store "contentstore-object-first-wins" [ "typ" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "same-object-hash" in
      let _ = Contentstore.save_object store ~hash ~content:"first" |> Result.expect ~msg:"first save_object should succeed" in
      let _ = Contentstore.save_object store ~hash ~content:"second" |> Result.expect ~msg:"second save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok "first" -> Ok ()
      | Ok other -> Error ("expected first object to win, got " ^ String.escaped other)
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_object_same_content_is_idempotent = fun _ctx ->
  with_store "contentstore-object-idempotent" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "same-content" in
      let content = "same content" in
      let _ = Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"first save_object should succeed" in
      let _ = Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"second save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok loaded when String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected duplicate save_object to preserve content"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_file_roundtrip = fun _ctx ->
  with_store "contentstore-save-file" [ "imports" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "file-roundtrip" in
      let source = Path.(tmpdir / Path.v "source.bin") in
      let content = "source\000file\255payload" in
      let _ = Fs.write content source |> Result.expect ~msg:"write source should succeed" in
      let _ = Contentstore.save_file store ~hash ~source |> Result.expect ~msg:"save_file should succeed" in
      match open_object_to_string store ~hash with
      | Ok loaded when String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected opened object to match imported file"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_object_creates_namespace_dir = fun _ctx ->
  with_store "contentstore-object-creates-namespace" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let namespace_dir = object_namespace_dir store in
      let exists_before = Fs.exists namespace_dir |> Result.expect ~msg:"exists should succeed" in
      let _ = Contentstore.save_object store ~hash:(Crypto.hash_string "creates-namespace") ~content:"payload"
      |> Result.expect ~msg:"save_object should succeed" in
      let exists_after = Fs.exists namespace_dir |> Result.expect ~msg:"exists should succeed" in
      if (not exists_before) && exists_after then
        Ok ()
      else
        Error "expected save_object to create the object namespace directory lazily")

let test_different_hashes_in_same_namespace_are_isolated = fun _ctx ->
  with_store "contentstore-object-distinct-hashes" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let left_hash = Crypto.hash_string "left-object" in
      let right_hash = Crypto.hash_string "right-object" in
      let _ = Contentstore.save_object store ~hash:left_hash ~content:"left"
      |> Result.expect ~msg:"save left object should succeed" in
      let _ = Contentstore.save_object store ~hash:right_hash ~content:"right"
      |> Result.expect ~msg:"save right object should succeed" in
      match (open_object_to_string store ~hash:left_hash, open_object_to_string store ~hash:right_hash) with
      | (Ok "left", Ok "right") -> Ok ()
      | _ -> Error "expected different hashes in one namespace to stay isolated")

let test_many_objects_in_one_namespace_remain_readable = fun _ctx ->
  with_store "contentstore-object-many" [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let rec write_all index =
        if index = 20 then
          Ok ()
        else
          let hash = Crypto.hash_string ("many:" ^ Int.to_string index) in
          let content = "payload:" ^ Int.to_string index in
          match Contentstore.save_object store ~hash ~content with
          | Ok () -> write_all (index + 1)
          | Error err -> Error (Contentstore.Store.error_message err)
      in
      let rec verify_all index =
        if index = 20 then
          Ok ()
        else
          let hash = Crypto.hash_string ("many:" ^ Int.to_string index) in
          let expected = "payload:" ^ Int.to_string index in
          match open_object_to_string store ~hash with
          | Ok loaded when String.equal loaded expected -> verify_all (index + 1)
          | Ok _ -> Error "expected all objects in one namespace to remain readable"
          | Error err -> Error (Contentstore.Store.error_message err)
      in
      match write_all 0 with
      | Error _ as err -> err
      | Ok () -> verify_all 0)

let test_unicode_namespace_roundtrip = fun _ctx ->
  Fs.with_tempdir ~prefix:"contentstore-object-unicode-namespace"
    (fun tmpdir ->
      let store =
        Contentstore.create
          ~root:Path.(tmpdir / Path.v "cache")
          ~ns:(namespace [ "módulos"; "東京" ])
          ~policy:Contentstore.Policy.default
      in
      let hash = Crypto.hash_string "unicode-namespace" in
      let _ = Contentstore.save_object store ~hash ~content:"payload"
      |> Result.expect ~msg:"save_object should succeed" in
      match open_object_to_string store ~hash with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected object in a unicode namespace to roundtrip"
      | Error err -> Error (Contentstore.Store.error_message err))
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let test_open_object_missing_is_structured = fun _ctx ->
  with_store "contentstore-open-object-missing" [ "typ" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "missing-object" in
      match Contentstore.open_object store ~hash with
      | Error (Contentstore.Store.Missing _) -> Ok ()
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok _ -> Error "expected open_object to fail for a missing object")

let test_save_file_rejects_directory_source = fun _ctx ->
  with_store "contentstore-save-file-invalid-source" [ "typ" ]
    (fun ~tmpdir ~store ->
      let source = Path.(tmpdir / Path.v "source-dir") in
      let _ = Fs.create_dir_all source |> Result.expect ~msg:"create source dir should succeed" in
      match Contentstore.save_file store ~hash:(Crypto.hash_string "source-dir") ~source with
      | Error (Contentstore.Store.Invalid_source_path { reason = Contentstore.Store.Source_not_file; _ }) -> Ok ()
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok () -> Error "expected save_file to reject a directory source")

let tests = [
  Test.case "save_object/open_object roundtrip" test_save_object_roundtrip;
  Test.case "save empty object/open_object roundtrip" test_save_empty_object_roundtrip;
  Test.case "save small ASCII object/open_object roundtrip" test_save_small_ascii_object_roundtrip;
  Test.case "save large object/open_object roundtrip" test_save_large_object_roundtrip;
  Test.case "save_object keeps first writer" test_save_object_first_writer_wins;
  Test.case "save_object with the same content is idempotent" test_save_object_same_content_is_idempotent;
  Test.case "save_file/open_object roundtrip" test_save_file_roundtrip;
  Test.case "save_object creates the namespace directory lazily" test_save_object_creates_namespace_dir;
  Test.case "different hashes in one namespace are isolated" test_different_hashes_in_same_namespace_are_isolated;
  Test.case "many objects in one namespace remain readable" test_many_objects_in_one_namespace_remain_readable;
  Test.case "unicode namespace object roundtrip" test_unicode_namespace_roundtrip;
  Test.case "open_object missing is structured" test_open_object_missing_is_structured;
  Test.case "save_file rejects directory source" test_save_file_rejects_directory_source;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_object_tests" ~tests ~args)
    ~args:Env.args
    ()
