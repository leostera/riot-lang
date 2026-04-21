open Std
module Test = Std.Test

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file |> Result.expect ~msg:"read_to_end should succeed" in
  let _ = Fs.File.close file |> Result.expect ~msg:"close should succeed" in
  content

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let with_stores = fun prefix fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let root = Path.(tmpdir / Path.v "cache") in
      let left = Contentstore.create ~root ~ns:(namespace [ "left" ]) ~policy:Contentstore.Policy.default in
      let right = Contentstore.create ~root ~ns:(namespace [ "right" ]) ~policy:Contentstore.Policy.default in
      fn ~left ~right) |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let open_object_to_string = fun store ~hash ->
  Contentstore.open_object store ~hash |> Result.map ~fn:read_opened_file

let open_named_object_to_string = fun store ~key ->
  Contentstore.open_named_object store ~key |> Result.map ~fn:read_opened_file

let test_hash_addressed_objects_are_namespaced = fun _ctx ->
  with_stores "contentstore-namespace-objects"
    (fun ~left ~right ->
      let hash = Crypto.hash_string "shared-hash" in
      let _ = Contentstore.save_object left ~hash ~content:"left" |> Result.expect ~msg:"left save should succeed" in
      let _ = Contentstore.save_object right ~hash ~content:"right" |> Result.expect ~msg:"right save should succeed" in
      match (open_object_to_string left ~hash, open_object_to_string right ~hash) with
      | (Ok "left", Ok "right") -> Ok ()
      | _ -> Error "expected namespaced stores to isolate hash-addressed objects")

let test_named_objects_are_namespaced = fun _ctx ->
  with_stores "contentstore-namespace-named"
    (fun ~left ~right ->
      let _ = Contentstore.save_named_object left ~key:"current" ~content:"left"
      |> Result.expect ~msg:"left save should succeed" in
      let _ = Contentstore.save_named_object right ~key:"current" ~content:"right"
      |> Result.expect ~msg:"right save should succeed" in
      match (
        open_named_object_to_string left ~key:"current",
        open_named_object_to_string right ~key:"current"
      ) with
      | (Ok "left", Ok "right") -> Ok ()
      | _ -> Error "expected namespaced stores to isolate named objects")

let tests = [
  Test.case "hash-addressed objects are namespaced" test_hash_addressed_objects_are_namespaced;
  Test.case "named objects are namespaced" test_named_objects_are_namespaced;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_namespace_isolation_tests" ~tests ~args ())
    ~args:Env.args
    ()
