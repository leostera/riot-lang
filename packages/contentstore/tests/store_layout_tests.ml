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

let dir_entries = fun path ->
  match Fs.read_dir path with
  | Ok entries -> Iter.MutIterator.to_list entries
  | Error _ -> []

let object_path = fun store hash ->
  let hex = Crypto.Digest.hex hash in
  Path.(Contentstore.root store
  / Path.v "objects"
  / Path.v "typ"
  / Path.v "modules"
  / Path.v (String.sub hex ~offset:0 ~len:2)
  / Path.v hex)

let named_object_path = fun store key ->
  let hash =
    Crypto.hash_string key
    |> Crypto.Digest.hex
  in
  Path.(Contentstore.root store
  / Path.v "named"
  / Path.v "plans"
  / Path.v "releases"
  / Path.v (String.sub hash ~offset:0 ~len:2)
  / Path.v hash)

let test_create_is_lazy = fun _ctx ->
  with_store
    "contentstore-layout-create-lazy"
    [ "modules" ]
    (fun ~tmpdir:_ ~store ->
      let exists_before =
        Fs.exists (Contentstore.root store)
        |> Result.expect ~msg:"exists should succeed"
      in
      let hash = Crypto.hash_string "lazy-write" in
      let _ =
        Contentstore.save_object store ~hash ~content:"hello"
        |> Result.expect ~msg:"save_object should succeed"
      in
      let exists_after =
        Fs.exists (Contentstore.root store)
        |> Result.expect ~msg:"exists should succeed"
      in
      if (not exists_before) && exists_after then
        Ok ()
      else
        Error "expected store root to be created lazily")

let test_hash_dir_layout = fun _ctx ->
  with_store
    "contentstore-layout-tree"
    [ "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "tree-layout" in
      let hex = Crypto.Digest.hex hash in
      let expected =
        Path.(Contentstore.root store
        / Path.v "trees"
        / Path.v (String.sub hex ~offset:0 ~len:2)
        / Path.v hex)
      in
      if Path.equal (Contentstore.hash_dir_of store hash) expected then
        Ok ()
      else
        Error "expected hash_dir_of to use the sharded trees layout")

let test_object_layout_is_sharded = fun _ctx ->
  with_store
    "contentstore-layout-object"
    [ "typ"; "modules" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "layout-object" in
      let _ =
        Contentstore.save_object store ~hash ~content:"hello"
        |> Result.expect ~msg:"save_object should succeed"
      in
      let path = object_path store hash in
      let exists =
        Fs.exists path
        |> Result.expect ~msg:"exists should succeed"
      in
      if exists then
        Ok ()
      else
        Error "expected object to live under objects/<namespace>/<shard>/<hash>")

let test_named_layout_is_sharded = fun _ctx ->
  with_store
    "contentstore-layout-named"
    [ "plans"; "releases" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_named_object store ~key:"latest" ~content:"hello"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      let path = named_object_path store "latest" in
      let exists =
        Fs.exists path
        |> Result.expect ~msg:"exists should succeed"
      in
      if exists then
        Ok ()
      else
        Error "expected named object to live under named/<namespace>/<shard>/<keyhash>")

let test_writes_leave_no_temp_files = fun _ctx ->
  with_store
    "contentstore-layout-temp-cleanup"
    [ "typ" ]
    (fun ~tmpdir:_ ~store ->
      let _ =
        Contentstore.save_object store ~hash:(Crypto.hash_string "object-temp") ~content:"one"
        |> Result.expect ~msg:"save_object should succeed"
      in
      let _ =
        Contentstore.save_named_object store ~key:"latest" ~content:"two"
        |> Result.expect ~msg:"save_named_object should succeed"
      in
      let tmp_root = Path.(Contentstore.root store / Path.v "tmp") in
      let immutable_entries = dir_entries Path.(tmp_root / Path.v "immutable") in
      let mutable_entries = dir_entries Path.(tmp_root / Path.v "mutable") in
      let tree_entries = dir_entries Path.(tmp_root / Path.v "trees") in
      if
        List.is_empty immutable_entries
        && List.is_empty mutable_entries
        && List.is_empty tree_entries
      then
        Ok ()
      else
        Error "expected successful writes to leave no temp files behind")

let tests = [
  Test.case "store root is created lazily" test_create_is_lazy;
  Test.case "hash_dir_of uses sharded tree layout" test_hash_dir_layout;
  Test.case "objects use sharded layout" test_object_layout_is_sharded;
  Test.case "named objects use sharded layout" test_named_layout_is_sharded;
  Test.case "successful writes leave no temp files behind" test_writes_leave_no_temp_files;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_layout_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
