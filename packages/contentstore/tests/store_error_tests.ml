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
  / Path.v "errors"
  / Path.v (String.sub hex ~offset:0 ~len:2))

let named_parent_dir = fun store key ->
  let key_hash =
    Crypto.hash_string key
    |> Crypto.Digest.hex
  in
  Path.(Contentstore.root store
  / Path.v "named"
  / Path.v "errors"
  / Path.v (String.sub key_hash ~offset:0 ~len:2))

let test_save_object_into_unwritable_namespace_is_structured = fun _ctx ->
  with_store
    "contentstore-error-save-object-perms"
    [ "errors" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "blocked-write" in
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
        | Error (Contentstore.Store.Io _) -> Ok ()
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected save_object to fail inside an unwritable namespace dir"
      in
      let _ = Fs.set_permissions blocked_dir Fs.Permissions.executable in
      result)

let test_save_named_object_into_unwritable_namespace_is_structured = fun _ctx ->
  with_store
    "contentstore-error-save-named-perms"
    [ "errors" ]
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
        | Error (Contentstore.Store.Io _) -> Ok ()
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected save_named_object to fail inside an unwritable namespace dir"
      in
      let _ = Fs.set_permissions blocked_dir Fs.Permissions.executable in
      result)

let test_open_object_permission_error_is_not_missing = fun _ctx ->
  with_store
    "contentstore-error-open-object-perms"
    [ "errors" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "permission-object" in
      let _ =
        Contentstore.save_object store ~hash ~content:"payload"
        |> Result.expect ~msg:"save_object should succeed"
      in
      let path = Path.(object_parent_dir store hash / Path.v (Crypto.Digest.hex hash)) in
      let _ =
        Fs.set_permissions path (Fs.Permissions.from_mode 0o000)
        |> Result.expect ~msg:"chmod object should succeed"
      in
      match Contentstore.open_object store ~hash with
      | Error (Contentstore.Store.Io _) -> Ok ()
      | Error (Contentstore.Store.Missing _) ->
          Error "expected permission-denied object read to stay distinct from missing"
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok _ -> Error "expected open_object to fail after permissions were removed")

let test_open_named_object_permission_error_is_not_missing = fun _ctx ->
  with_store
    "contentstore-error-open-named-perms"
    [ "errors" ]
    (fun ~tmpdir:_ ~store ->
      let key = "current" in
      let _ =
        Contentstore.save_named_object store ~key ~content:"payload"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      let key_hash =
        Crypto.hash_string key
        |> Crypto.Digest.hex
      in
      let path = Path.(named_parent_dir store key / Path.v key_hash) in
      let _ =
        Fs.set_permissions path (Fs.Permissions.from_mode 0o000)
        |> Result.expect ~msg:"chmod named object should succeed"
      in
      match Contentstore.open_named_object store ~key with
      | Error (Contentstore.Store.Io _) -> Ok ()
      | Error (Contentstore.Store.Missing _) ->
          Error "expected permission-denied named read to stay distinct from missing"
      | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
      | Ok _ -> Error "expected open_named_object to fail after permissions were removed")

let test_commit_dir_failure_reports_context = fun _ctx ->
  with_store
    "contentstore-error-commit-dir-context"
    [ "errors" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "commit-dir-context" in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let trees_root = Path.(Contentstore.root store / Path.v "trees") in
      let _ =
        Fs.create_dir_all source_dir
        |> Result.expect ~msg:"create source dir should succeed"
      in
      let _ =
        Fs.write "payload" Path.(source_dir / Path.v "payload.txt")
        |> Result.expect ~msg:"write payload should succeed"
      in
      let _ =
        Fs.create_dir_all trees_root
        |> Result.expect ~msg:"create trees root should succeed"
      in
      let _ =
        Fs.set_permissions trees_root (Fs.Permissions.from_mode 0o555)
        |> Result.expect ~msg:"chmod trees root should succeed"
      in
      let result =
        match Contentstore.commit_dir store ~hash ~source_dir with
        | Error (Contentstore.Store.Io { path; related_path = Some _; _ }) ->
            if Path.equal path (Contentstore.hash_dir_of store hash) then
              Ok ()
            else
              Error "expected commit_dir failure to report the destination path"
        | Error err -> Error ("unexpected error: " ^ Contentstore.Store.error_message err)
        | Ok () -> Error "expected commit_dir to fail when the trees root is unwritable"
      in
      let _ = Fs.set_permissions trees_root Fs.Permissions.executable in
      result)

let tests = [
  Test.case
    "save_object into unwritable namespace is structured"
    test_save_object_into_unwritable_namespace_is_structured;
  Test.case
    "save_named_object into unwritable namespace is structured"
    test_save_named_object_into_unwritable_namespace_is_structured;
  Test.case
    "open_object permission error is not missing"
    test_open_object_permission_error_is_not_missing;
  Test.case
    "open_named_object permission error is not missing"
    test_open_named_object_permission_error_is_not_missing;
  Test.case "commit_dir failure reports structured context" test_commit_dir_failure_reports_context;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_error_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
