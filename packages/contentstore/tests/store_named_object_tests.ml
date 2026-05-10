open Std

module Test = Std.Test

let read_opened_file = fun file ->
  let content =
    Fs.File.read_to_end file
    |> Result.expect ~msg:"read_to_end should succeed"
  in
  let _ =
    Fs.File.close file
    |> Result.expect ~msg:"close should succeed"
  in
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
  Fs.with_tempdir ~prefix (fun tmpdir -> fn ~tmpdir ~store:(make_store tmpdir parts))
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let open_named_object_to_string = fun store ~key ->
  Contentstore.open_named_object store ~key
  |> Result.map ~fn:read_opened_file

let named_namespace_dir = fun store ->
  Path.(Contentstore.root store / Path.v "named" / Path.v "plans")

let test_save_named_object_overwrites = fun _ctx ->
  with_store
    "contentstore-save-named-object"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"Colors" ~content:"first"
        |> Result.expect ~msg:"first save_named_object should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"Colors" ~content:"second"
        |> Result.expect ~msg:"second save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"Colors" with
      | Ok "second" -> Ok ()
      | Ok other -> Error ("expected latest named object to win, got " ^ String.escaped other)
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_named_empty_object_roundtrip = fun _ctx ->
  with_store
    "contentstore-save-named-empty-object"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"empty" ~content:""
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"empty" with
      | Ok "" -> Ok ()
      | Ok _ -> Error "expected empty named object to stay empty"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_named_binary_object_roundtrip = fun _ctx ->
  with_store
    "contentstore-save-named-binary-object"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let content = "hello\000world\255" in
      let _ =
        Contentstore.save_named_object store ~key:"binary" ~content
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"binary" with
      | Ok loaded when String.equal loaded content -> Ok ()
      | Ok _ -> Error "expected binary named object to roundtrip"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_named_file_overwrites = fun _ctx ->
  with_store
    "contentstore-save-named-file"
    [ "plans" ]
    (fun ~tmpdir ~store ->
      let first = Path.(tmpdir / Path.v "first.bin") in
      let second = Path.(tmpdir / Path.v "second.bin") in
      let _ =
        Fs.write "first" first
        |> Result.expect ~msg:"write first source should succeed"
      in
      let _ =
        Fs.write "second" second
        |> Result.expect ~msg:"write second source should succeed"
      in
      let _ =
        Contentstore.save_named_file store ~key:"active" ~source:first
        |> Result.expect ~msg:"first save_named_file should succeed"
      in
      let _ =
        Contentstore.save_named_file store ~key:"active" ~source:second
        |> Result.expect ~msg:"second save_named_file should succeed"
      in
      match open_named_object_to_string store ~key:"active" with
      | Ok "second" -> Ok ()
      | Ok other -> Error ("expected latest named file to win, got " ^ String.escaped other)
      | Error err -> Error (Contentstore.Store.error_message err))

let test_different_named_keys_are_isolated = fun _ctx ->
  with_store
    "contentstore-save-named-distinct-keys"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"left" ~content:"left"
        |> Result.expect ~msg:"save left named object should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"right" ~content:"right"
        |> Result.expect ~msg:"save right named object should succeed"
      in
      match (
        open_named_object_to_string store ~key:"left",
        open_named_object_to_string store ~key:"right"
      ) with
      | (Ok "left", Ok "right") -> Ok ()
      | _ -> Error "expected different named keys to remain isolated")

let test_save_named_object_creates_namespace_dir = fun _ctx ->
  with_store
    "contentstore-save-named-creates-namespace"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let namespace_dir = named_namespace_dir store in
      let exists_before =
        Fs.exists namespace_dir
        |> Result.expect ~msg:"exists should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"latest" ~content:"payload"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      let exists_after =
        Fs.exists namespace_dir
        |> Result.expect ~msg:"exists should succeed"
      in
      if (not exists_before) && exists_after then
        Ok ()
      else
        Error "expected save_named_object to create the named namespace directory lazily")

let test_named_unicode_key_roundtrip = fun _ctx ->
  with_store
    "contentstore-save-named-unicode-key"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let key = "現在" in
      let _ =
        Contentstore.save_named_object store ~key ~content:"payload"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      match open_named_object_to_string store ~key with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected unicode named key to roundtrip"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_named_long_key_roundtrip = fun _ctx ->
  with_store
    "contentstore-save-named-long-key"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let key = String.init ~len:2_048 ~fn:(fun _ -> 'k') in
      let _ =
        Contentstore.save_named_object store ~key ~content:"payload"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      match open_named_object_to_string store ~key with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected long named key to roundtrip"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_named_punctuation_key_roundtrip = fun _ctx ->
  with_store
    "contentstore-save-named-punctuation-key"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let key = "release@2026:04:13?ok=yes#x/y" in
      let _ =
        Contentstore.save_named_object store ~key ~content:"payload"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      match open_named_object_to_string store ~key with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected punctuation named key to roundtrip"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_save_named_same_content_is_idempotent = fun _ctx ->
  with_store
    "contentstore-save-named-idempotent"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:"payload"
        |> Result.expect ~msg:"first save_named_object should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:"payload"
        |> Result.expect ~msg:"second save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"current" with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected duplicate named save to preserve content"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_named_overwrite_with_larger_content = fun _ctx ->
  with_store
    "contentstore-save-named-larger-content"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:"small"
        |> Result.expect ~msg:"first save_named_object should succeed"
      in
      let larger = String.init ~len:8_192 ~fn:(fun _ -> 'x') in
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:larger
        |> Result.expect ~msg:"second save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"current" with
      | Ok loaded when String.equal loaded larger -> Ok ()
      | Ok _ -> Error "expected larger overwrite to remain fully readable"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_named_overwrite_with_smaller_content = fun _ctx ->
  with_store
    "contentstore-save-named-smaller-content"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let larger = String.init ~len:8_192 ~fn:(fun _ -> 'x') in
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:larger
        |> Result.expect ~msg:"first save_named_object should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"current" ~content:"small"
        |> Result.expect ~msg:"second save_named_object should succeed"
      in
      match open_named_object_to_string store ~key:"current" with
      | Ok "small" -> Ok ()
      | Ok _ -> Error "expected smaller overwrite to remain fully readable"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_failed_named_overwrite_preserves_previous_value = fun _ctx ->
  with_store
    "contentstore-save-named-failed-overwrite"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      let key = "current" in
      let _ =
        Contentstore.save_named_object store ~key ~content:"first"
        |> Result.expect ~msg:"initial save_named_object should succeed"
      in
      let key_hash =
        Crypto.hash_string key
        |> Crypto.Digest.hex
      in
      let parent_dir =
        Path.(Contentstore.root store
        / Path.v "named"
        / Path.v "plans"
        / Path.v (String.sub key_hash ~offset:0 ~len:2))
      in
      let _ =
        Fs.set_permissions parent_dir (Fs.Permissions.from_mode 0o555)
        |> Result.expect ~msg:"chmod shard dir should succeed"
      in
      let result =
        match Contentstore.save_named_object store ~key ~content:"second" with
        | Error (Contentstore.Store.Io _) ->
            let _ = Fs.set_permissions parent_dir Fs.Permissions.executable in
            (match open_named_object_to_string store ~key with
            | Ok "first" -> Ok ()
            | Ok _ -> Error "expected failed overwrite to preserve the previous named value"
            | Error err -> Error (Contentstore.Store.error_message err))
        | Error err ->
            let _ = Fs.set_permissions parent_dir Fs.Permissions.executable in
            Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () ->
            let _ = Fs.set_permissions parent_dir Fs.Permissions.executable in
            Error "expected save_named_object overwrite to fail in an unwritable shard dir"
      in
      result)

let test_named_missing_is_structured = fun _ctx ->
  with_store
    "contentstore-open-named-object-missing"
    [ "plans" ]
    (fun ~tmpdir:_ ~store ->
      match Contentstore.open_named_object store ~key:"missing" with
      | Error (Contentstore.Store.Missing _) -> Ok ()
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok _ -> Error "expected open_named_object to fail for a missing object")

let tests = [
  Test.case "save_named_object keeps last writer" test_save_named_object_overwrites;
  Test.case
    "save empty named object/open_named_object roundtrip"
    test_save_named_empty_object_roundtrip;
  Test.case
    "save binary named object/open_named_object roundtrip"
    test_save_named_binary_object_roundtrip;
  Test.case "save_named_file keeps last writer" test_save_named_file_overwrites;
  Test.case "different named keys are isolated" test_different_named_keys_are_isolated;
  Test.case
    "save_named_object creates the namespace directory lazily"
    test_save_named_object_creates_namespace_dir;
  Test.case "unicode named key roundtrip" test_named_unicode_key_roundtrip;
  Test.case "long named key roundtrip" test_named_long_key_roundtrip;
  Test.case "punctuation named key roundtrip" test_named_punctuation_key_roundtrip;
  Test.case
    "save_named_object with the same content is idempotent"
    test_save_named_same_content_is_idempotent;
  Test.case
    "named overwrite with larger content stays readable"
    test_named_overwrite_with_larger_content;
  Test.case
    "named overwrite with smaller content stays readable"
    test_named_overwrite_with_smaller_content;
  Test.case
    "failed named overwrite preserves the previous value"
    test_failed_named_overwrite_preserves_previous_value;
  Test.case "open_named_object missing is structured" test_named_missing_is_structured;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_named_object_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
