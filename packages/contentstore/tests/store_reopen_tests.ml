open Std
module Test = Std.Test

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file |> Result.expect ~msg:"read_to_end should succeed" in
  let _ = Fs.File.close file |> Result.expect ~msg:"close should succeed" in
  content

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let create_store = fun root parts ->
  Contentstore.create ~root ~ns:(namespace parts) ~policy:Contentstore.Policy.default

let with_root = fun prefix fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let root = Path.(tmpdir / Path.v "cache") in
      fn ~tmpdir ~root) |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let test_reopen_sees_previously_saved_object = fun _ctx ->
  with_root "contentstore-reopen-object"
    (fun ~tmpdir:_ ~root ->
      let hash = Crypto.hash_string "reopen-object" in
      let writer = create_store root [ "objects" ] in
      let _ = Contentstore.save_object writer ~hash ~content:"payload" |> Result.expect ~msg:"save_object should succeed" in
      let reader = create_store root [ "objects" ] in
      match Contentstore.open_object reader ~hash |> Result.map ~fn:read_opened_file with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected reopened store to read the previously saved object"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_reopen_sees_previously_saved_named_object = fun _ctx ->
  with_root "contentstore-reopen-named"
    (fun ~tmpdir:_ ~root ->
      let writer = create_store root [ "named" ] in
      let _ = Contentstore.save_named_object writer ~key:"current" ~content:"payload"
      |> Result.expect ~msg:"save_named_object should succeed" in
      let reader = create_store root [ "named" ] in
      match Contentstore.open_named_object reader ~key:"current" |> Result.map ~fn:read_opened_file with
      | Ok "payload" -> Ok ()
      | Ok _ -> Error "expected reopened store to read the previously saved named object"
      | Error err -> Error (Contentstore.Store.error_message err))

let test_reopen_sees_previously_committed_tree = fun _ctx ->
  with_root "contentstore-reopen-tree"
    (fun ~tmpdir ~root ->
      let hash = Crypto.hash_string "reopen-tree" in
      let writer = create_store root [ "trees" ] in
      let source_dir = Path.(tmpdir / Path.v "source") in
      let _ = Fs.create_dir_all Path.(source_dir / Path.v "nested") |> Result.expect ~msg:"create source dir should succeed" in
      let _ = Fs.write "hello" Path.(source_dir / Path.v "nested" / Path.v "payload.txt")
      |> Result.expect ~msg:"write payload should succeed" in
      let _ = Contentstore.commit_dir writer ~hash ~source_dir |> Result.expect ~msg:"commit_dir should succeed" in
      let reader = create_store root [ "trees" ] in
      match Fs.read_to_string
        Path.(Contentstore.hash_dir_of reader hash / Path.v "nested" / Path.v "payload.txt") with
      | Ok "hello" -> Ok ()
      | Ok _ -> Error "expected reopened store to preserve committed tree contents"
      | Error _ -> Error "expected committed tree to remain readable after reopen")

let tests = [
  Test.case "reopen sees previously saved object" test_reopen_sees_previously_saved_object;
  Test.case "reopen sees previously saved named object" test_reopen_sees_previously_saved_named_object;
  Test.case "reopen sees previously committed tree" test_reopen_sees_previously_committed_tree;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_reopen_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
