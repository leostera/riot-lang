open Std

module Test = Std.Test

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

let object_parent_dir = fun store hash ->
  let hex = Crypto.Digest.hex hash in
  Path.(Contentstore.root store
  / Path.v "objects"
  / Path.v "cleanup"
  / Path.v (String.sub hex ~offset:0 ~len:2))

let named_parent_dir = fun store key ->
  let key_hash =
    Crypto.hash_string key
    |> Crypto.Digest.hex
  in
  Path.(Contentstore.root store
  / Path.v "named"
  / Path.v "cleanup"
  / Path.v (String.sub key_hash ~offset:0 ~len:2))

let scope_entries = fun store scope ->
  let dir = Path.(Contentstore.root store / Path.v "tmp" / Path.v scope) in
  match Fs.read_dir dir with
  | Ok entries -> Iter.MutIterator.to_list entries
  | Error _ -> []

let test_failed_save_object_cleans_immutable_temp_files = fun _ctx ->
  with_store
    "contentstore-temp-cleanup-object"
    [ "cleanup" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "blocked-object" in
      let blocked_dir = object_parent_dir store hash in
      let _ =
        Fs.create_dir_all blocked_dir
        |> Result.expect ~msg:"create blocked dir should succeed"
      in
      let _ =
        Fs.set_permissions blocked_dir (Fs.Permissions.from_mode 0o555)
        |> Result.expect ~msg:"chmod blocked dir should succeed"
      in
      let result =
        match Contentstore.save_object store ~hash ~content:"payload" with
        | Error (Contentstore.Store.Io _) -> (
            let entries = scope_entries store "immutable" in
            if List.is_empty entries then
              Ok ()
            else
              Error "expected failed save_object to clean immutable temp files"
          )
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected save_object to fail inside an unwritable destination shard"
      in
      let _ = Fs.set_permissions blocked_dir Fs.Permissions.executable in
      result)

let test_failed_save_named_object_cleans_mutable_temp_files = fun _ctx ->
  with_store
    "contentstore-temp-cleanup-named"
    [ "cleanup" ]
    (fun ~tmpdir:_ ~store ->
      let key = "current" in
      let blocked_dir = named_parent_dir store key in
      let _ =
        Fs.create_dir_all blocked_dir
        |> Result.expect ~msg:"create blocked dir should succeed"
      in
      let _ =
        Fs.set_permissions blocked_dir (Fs.Permissions.from_mode 0o555)
        |> Result.expect ~msg:"chmod blocked dir should succeed"
      in
      let result =
        match Contentstore.save_named_object store ~key ~content:"payload" with
        | Error (Contentstore.Store.Io _) -> (
            let entries = scope_entries store "mutable" in
            if List.is_empty entries then
              Ok ()
            else
              Error "expected failed save_named_object to clean mutable temp files"
          )
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected save_named_object to fail inside an unwritable destination shard"
      in
      let _ = Fs.set_permissions blocked_dir Fs.Permissions.executable in
      result)

let test_failed_save_file_cleans_immutable_temp_files = fun _ctx ->
  with_store
    "contentstore-temp-cleanup-save-file"
    [ "cleanup" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "blocked-file" in
      let blocked_dir = object_parent_dir store hash in
      let source = Path.(tmpdir / Path.v "source.bin") in
      let _ =
        Fs.write "payload" source
        |> Result.expect ~msg:"write source should succeed"
      in
      let _ =
        Fs.create_dir_all blocked_dir
        |> Result.expect ~msg:"create blocked dir should succeed"
      in
      let _ =
        Fs.set_permissions blocked_dir (Fs.Permissions.from_mode 0o555)
        |> Result.expect ~msg:"chmod blocked dir should succeed"
      in
      let result =
        match Contentstore.save_file store ~hash ~source with
        | Error (Contentstore.Store.Io _) -> (
            let entries = scope_entries store "immutable" in
            if List.is_empty entries then
              Ok ()
            else
              Error "expected failed save_file to clean immutable temp files"
          )
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected save_file to fail inside an unwritable destination shard"
      in
      let _ = Fs.set_permissions blocked_dir Fs.Permissions.executable in
      result)

let tests = [
  Test.case
    "failed save_object cleans immutable temp files"
    test_failed_save_object_cleans_immutable_temp_files;
  Test.case
    "failed save_named_object cleans mutable temp files"
    test_failed_save_named_object_cleans_mutable_temp_files;
  Test.case
    "failed save_file cleans immutable temp files"
    test_failed_save_file_cleans_immutable_temp_files;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_temp_cleanup_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
