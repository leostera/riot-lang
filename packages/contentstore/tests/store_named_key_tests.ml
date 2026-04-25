open Std

module Test = Std.Test

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file |> Result.expect ~msg:"read_to_end should succeed" in
  let _ = Fs.File.close file |> Result.expect ~msg:"close should succeed" in content

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let make_store = fun tmpdir parts -> Contentstore.create ~root:Path.(tmpdir / Path.v "cache") ~ns:(namespace parts) ~policy:Contentstore.Policy.default

let with_store = fun prefix parts fn ->
  Fs.with_tempdir ~prefix
    (
      fun tmpdir -> fn ~tmpdir ~store:(make_store tmpdir parts)
    ) |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let expect_named_roundtrip = fun store ~key ->
  let _ = Contentstore.save_named_object store ~key ~content:"payload" |> Result.expect ~msg:"save_named_object should succeed" in
  match Contentstore.open_named_object store ~key |> Result.map ~fn:read_opened_file with
  | Ok "payload" -> Ok ()
  | Ok _ -> Error "expected named key to roundtrip safely"
  | Error err -> Error (Contentstore.Store.error_message err)

let test_named_slash_key_roundtrip = fun _ctx ->
  with_store "contentstore-named-key-slash" [ "named-keys" ]
    (
      fun ~tmpdir:_ ~store -> expect_named_roundtrip store ~key:"a/b"
    )

let test_named_backslash_key_roundtrip = fun _ctx ->
  with_store "contentstore-named-key-backslash" [ "named-keys" ]
    (
      fun ~tmpdir:_ ~store -> expect_named_roundtrip store ~key:"a\\b"
    )

let test_named_dot_key_roundtrip = fun _ctx ->
  with_store "contentstore-named-key-dot" [ "named-keys" ]
    (
      fun ~tmpdir:_ ~store -> expect_named_roundtrip store ~key:"."
    )

let test_named_dotdot_key_roundtrip = fun _ctx ->
  with_store "contentstore-named-key-dotdot" [ "named-keys" ]
    (
      fun ~tmpdir:_ ~store -> expect_named_roundtrip store ~key:".."
    )

let test_named_empty_key_roundtrip = fun _ctx ->
  with_store "contentstore-named-key-empty" [ "named-keys" ]
    (
      fun ~tmpdir:_ ~store -> expect_named_roundtrip store ~key:""
    )

let tests =
  [
    Test.case "slash named key roundtrip" test_named_slash_key_roundtrip;
    Test.case "backslash named key roundtrip" test_named_backslash_key_roundtrip;
    Test.case "dot named key roundtrip" test_named_dot_key_roundtrip;
    Test.case "dotdot named key roundtrip" test_named_dotdot_key_roundtrip;
    Test.case "empty named key roundtrip" test_named_empty_key_roundtrip;
  ]

let main ~args = Test.Cli.main ~name:"contentstore_store_named_key_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
