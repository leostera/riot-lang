open Std
open Propane

module Test = Std.Test

let examples = 500

let property_config = { Property.default_config with test_count = examples }

let distinct_string_triple_arb =
  let gen =
    Generator.map
      (fun (left_key, right_key, content) ->
        if String.equal left_key right_key then
          (left_key, right_key ^ "\x00", content)
        else
          (left_key, right_key, content))
      (Generator.triple Generator.string Generator.string Generator.string)
  in
  Arbitrary.make ~print:(Printer.triple Printer.string Printer.string Printer.string) gen

let string_non_empty_list_pair_arb =
  let gen = Generator.pair Generator.string (Generator.non_empty_list Generator.string) in
  Arbitrary.make ~print:(Printer.pair Printer.string (Printer.list Printer.string)) gen

let assert_property = fun name property ->
  match Property.check ~config:property_config property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (name
      ^ " failed\nCounter-example:\n"
      ^ counter_example
      ^ "\nShrink steps: "
      ^ Int.to_string shrink_steps)
  | Property.Error { exception_ = _; backtrace } ->
      Error (name ^ " raised an unexpected exception\n" ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

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

let with_store = fun prefix parts fn ->
  Fs.with_tempdir
    ~prefix
    (fun tmpdir ->
      let store =
        Contentstore.create
          ~root:Path.(tmpdir / Path.v "cache")
          ~ns:(namespace parts)
          ~policy:Contentstore.Policy.default
      in
      fn ~tmpdir ~store)
  |> Result.unwrap_or ~default:false

let named_objects_keep_last_writer =
  Property.for_all
    Arbitrary.(pair string string)
    (fun (first, second) ->
      with_store
        "contentstore-prop-named-last-wins"
        [ "named" ]
        (fun ~tmpdir:_ ~store ->
          let _ =
            Contentstore.save_named_object store ~key:"current" ~content:first
            |> Result.expect ~msg:"first save_named_object should succeed"
          in
          let _ =
            Contentstore.save_named_object store ~key:"current" ~content:second
            |> Result.expect ~msg:"second save_named_object should succeed"
          in
          match Contentstore.open_named_object store ~key:"current"
          |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded second
          | Error _ -> false))

let save_named_object_roundtrip =
  Property.for_all
    Arbitrary.(pair string string)
    (fun (key, content) ->
      with_store
        "contentstore-prop-named-roundtrip"
        [ "named" ]
        (fun ~tmpdir:_ ~store ->
          let _ =
            Contentstore.save_named_object store ~key ~content
            |> Result.expect ~msg:"save_named_object should succeed"
          in
          match Contentstore.open_named_object store ~key
          |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded content
          | Error _ -> false))

let distinct_named_keys_are_isolated =
  Property.for_all
    distinct_string_triple_arb
    (fun (left_key, right_key, content) ->
      with_store
        "contentstore-prop-named-keys"
        [ "named" ]
        (fun ~tmpdir:_ ~store ->
          let left_content = "left:" ^ content in
          let right_content = "right:" ^ content in
          let _ =
            Contentstore.save_named_object store ~key:left_key ~content:left_content
            |> Result.expect ~msg:"save left named object should succeed"
          in
          let _ =
            Contentstore.save_named_object store ~key:right_key ~content:right_content
            |> Result.expect ~msg:"save right named object should succeed"
          in
          match (
            Contentstore.open_named_object store ~key:left_key
            |> Result.map ~fn:read_opened_file,
            Contentstore.open_named_object store ~key:right_key
            |> Result.map ~fn:read_opened_file
          ) with
          | (Ok loaded_left, Ok loaded_right) ->
              String.equal loaded_left left_content && String.equal loaded_right right_content
          | _ -> false))

let named_writes_survive_reopen =
  Property.for_all
    string_non_empty_list_pair_arb
    (fun (key, contents) ->
      with_store
        "contentstore-prop-named-reopen"
        [ "named" ]
        (fun ~tmpdir:_ ~store ->
          let root = Contentstore.root store in
          let rec write_all latest remaining =
            match remaining with
            | [] -> latest
            | content :: rest ->
                let _ =
                  Contentstore.save_named_object store ~key ~content
                  |> Result.expect ~msg:"save_named_object should succeed"
                in
                write_all content rest
          in
          let latest = write_all "" contents in
          let reopened =
            Contentstore.create
              ~root
              ~ns:(namespace [ "named" ])
              ~policy:Contentstore.Policy.default
          in
          match Contentstore.open_named_object reopened ~key
          |> Result.map ~fn:read_opened_file with
          | Ok loaded -> String.equal loaded latest
          | Error _ -> false))

let tests = [
  Test.property
    "save_named_object keeps last writer"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property
        "save_named_object keeps last writer"
        named_objects_keep_last_writer);
  Test.property
    "save_named_object/open_named_object roundtrip"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property
        "save_named_object/open_named_object roundtrip"
        save_named_object_roundtrip);
  Test.property
    "distinct named keys are isolated"
    ~size:Large
    ~examples
    (fun _ctx -> assert_property "distinct named keys are isolated" distinct_named_keys_are_isolated);
  Test.property
    "named writes survive reopen"
    ~size:Large
    ~examples
    (fun _ctx -> assert_property "named writes survive reopen" named_writes_survive_reopen);
]

let main ~args =
  Test.Cli.main ~name:"contentstore_store_named_object_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
