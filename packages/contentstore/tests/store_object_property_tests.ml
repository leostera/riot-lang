open Std
open Propane
module Test = Std.Test

let examples = 500

let property_config = { Property.default_config with test_count = examples }

let assert_property = fun name property ->
  match Property.check ~config:property_config property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } -> Error (name
  ^ " failed\nCounter-example:\n"
  ^ counter_example
  ^ "\nShrink steps: "
  ^ Int.to_string shrink_steps)
  | Property.Error { exception_=_; backtrace } -> Error (name
  ^ " raised an unexpected exception\n"
  ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file |> Result.expect ~msg:"read_to_end should succeed" in
  let _ = Fs.File.close file |> Result.expect ~msg:"close should succeed" in
  content

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let with_store = fun prefix parts fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let store = Contentstore.create
        ~root:Path.(tmpdir / Path.v "cache")
        ~ns:(namespace parts)
        ~policy:Contentstore.Policy.default in
      fn ~tmpdir ~store) |> Result.unwrap_or ~default:false

let save_object_roundtrip =
  Property.for_all Arbitrary.string
    (fun content ->
      with_store "contentstore-prop-object-roundtrip" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let hash = Crypto.hash_string ("object:" ^ content) in
          let _ = Contentstore.save_object store ~hash ~content |> Result.expect ~msg:"save_object should succeed" in
          match Contentstore.open_object store ~hash |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded content
          | Error _ -> false))

let save_file_roundtrip =
  Property.for_all Arbitrary.string
    (fun content ->
      with_store "contentstore-prop-save-file" [ "imports" ]
        (fun ~tmpdir ~store ->
          let hash = Crypto.hash_string ("file:" ^ content) in
          let source = Path.(tmpdir / Path.v "source.bin") in
          let _ = Fs.write content source |> Result.expect ~msg:"write source should succeed" in
          let _ = Contentstore.save_file store ~hash ~source |> Result.expect ~msg:"save_file should succeed" in
          match Contentstore.open_object store ~hash |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded content
          | Error _ -> false))

let save_object_is_idempotent =
  Property.for_all Arbitrary.string
    (fun content ->
      with_store "contentstore-prop-object-idempotent" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let hash = Crypto.hash_string ("idempotent:" ^ content) in
          let _ = Contentstore.save_object store ~hash ~content |> Result.expect ~msg:"first save_object should succeed" in
          let _ = Contentstore.save_object store ~hash ~content |> Result.expect ~msg:"second save_object should succeed" in
          match Contentstore.open_object store ~hash |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded content
          | Error _ -> false))

let object_roundtrip_survives_reopen =
  Property.for_all Arbitrary.string
    (fun content ->
      with_store "contentstore-prop-object-reopen" [ "objects" ]
        (fun ~tmpdir ~store ->
          let hash = Crypto.hash_string ("reopen:" ^ content) in
          let root = Contentstore.root store in
          let _ = Contentstore.save_object store ~hash ~content |> Result.expect ~msg:"save_object should succeed" in
          let reopened = Contentstore.create
            ~root
            ~ns:(namespace [ "objects" ])
            ~policy:Contentstore.Policy.default in
          match Contentstore.open_object reopened ~hash |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded content
          | Error _ -> false))

let successful_object_writes_leave_no_temp_files =
  Property.for_all Arbitrary.(list string)
    (fun contents ->
      with_store "contentstore-prop-object-no-temp" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let rec save_all index items =
            match items with
            | [] -> true
            | content :: rest ->
                let hash = Crypto.hash_string ("temp:" ^ Int.to_string index ^ ":" ^ content) in
                match Contentstore.save_object store ~hash ~content with
                | Ok () -> save_all (index + 1) rest
                | Error _ -> false
          in
          let writes_ok = save_all 0 contents in
          let tmp_root = Path.(Contentstore.root store / Path.v "tmp" / Path.v "immutable") in
          let entries =
            match Fs.read_dir tmp_root with
            | Ok iter -> Iter.MutIterator.to_list iter
            | Error _ -> []
          in
          writes_ok && List.is_empty entries))

let tests = [
  Test.property
    "save_object/open_object roundtrip"
    ~examples
    (fun _ctx -> assert_property "save_object/open_object roundtrip" save_object_roundtrip);
  Test.property
    "save_file/open_object roundtrip"
    ~size:Large
    ~examples
    (fun _ctx -> assert_property "save_file/open_object roundtrip" save_file_roundtrip);
  Test.property
    "save_object is idempotent for the same hash and content"
    ~size:Large
    ~examples
    (fun _ctx -> assert_property "save_object is idempotent for the same hash and content" save_object_is_idempotent);
  Test.property
    "object roundtrip survives reopen"
    ~examples
    (fun _ctx -> assert_property "object roundtrip survives reopen" object_roundtrip_survives_reopen);
  Test.property
    "successful object writes leave no temp files"
    ~size:Large
    ~examples
    (fun _ctx -> assert_property "successful object writes leave no temp files" successful_object_writes_leave_no_temp_files);
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_object_property_tests" ~tests ~args ())
    ~args:Env.args
    ()
