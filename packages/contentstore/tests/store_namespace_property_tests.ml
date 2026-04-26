open Std
open Propane

module Test = Std.Test

let examples = 300

let property_config = { Property.default_config with test_count = examples }

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

let abs_int = fun value ->
  if value < 0 then
    0 - value
  else
    value

let namespace_name = fun id -> "ns-" ^ Int.to_string (abs_int id)

let namespace_for_id = fun id ->
  Contentstore.Namespace.from_parts [ namespace_name id ]
  |> Result.expect ~msg:"generated namespace should be valid"

let with_stores = fun prefix left_id right_id fn ->
  Fs.with_tempdir
    ~prefix
    (fun tmpdir ->
      let root = Path.(tmpdir / Path.v "cache") in
      let left =
        Contentstore.create ~root ~ns:(namespace_for_id left_id) ~policy:Contentstore.Policy.default
      in
      let right =
        Contentstore.create
          ~root
          ~ns:(namespace_for_id right_id)
          ~policy:Contentstore.Policy.default
      in
      fn ~left ~right)
  |> Result.unwrap_or ~default:false

let object_writes_stay_in_their_namespace =
  Property.for_all
    Arbitrary.(triple int int string)
    (fun (left_id, right_id, content) ->
      Property.assume (namespace_name left_id != namespace_name right_id);
      with_stores
        "contentstore-prop-namespace-objects"
        left_id
        right_id
        (fun ~left ~right ->
          let hash = Crypto.hash_string "shared-object-hash" in
          let left_content = "left:" ^ content in
          let right_content = "right:" ^ content in
          let _ =
            Contentstore.save_object left ~hash ~content:left_content
            |> Result.expect ~msg:"left save_object should succeed"
          in
          let _ =
            Contentstore.save_object right ~hash ~content:right_content
            |> Result.expect ~msg:"right save_object should succeed"
          in
          match (
            Contentstore.open_object left ~hash
            |> Result.map ~fn:read_opened_file,
            Contentstore.open_object right ~hash
            |> Result.map ~fn:read_opened_file
          ) with
          | (Ok loaded_left, Ok loaded_right) ->
              String.equal loaded_left left_content && String.equal loaded_right right_content
          | _ -> false))

let named_writes_stay_in_their_namespace =
  Property.for_all
    Arbitrary.(triple int int string)
    (fun (left_id, right_id, content) ->
      Property.assume (namespace_name left_id != namespace_name right_id);
      with_stores
        "contentstore-prop-namespace-named"
        left_id
        right_id
        (fun ~left ~right ->
          let key = "current" in
          let left_content = "left:" ^ content in
          let right_content = "right:" ^ content in
          let _ =
            Contentstore.save_named_object left ~key ~content:left_content
            |> Result.expect ~msg:"left save_named_object should succeed"
          in
          let _ =
            Contentstore.save_named_object right ~key ~content:right_content
            |> Result.expect ~msg:"right save_named_object should succeed"
          in
          match (
            Contentstore.open_named_object left ~key
            |> Result.map ~fn:read_opened_file,
            Contentstore.open_named_object right ~key
            |> Result.map ~fn:read_opened_file
          ) with
          | (Ok loaded_left, Ok loaded_right) ->
              String.equal loaded_left left_content && String.equal loaded_right right_content
          | _ -> false))

let tests = [
  Test.property
    "object writes stay in their namespace"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property "object writes stay in their namespace" object_writes_stay_in_their_namespace);
  Test.property
    "named writes stay in their namespace"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property "named writes stay in their namespace" named_writes_stay_in_their_namespace);
]

let main ~args = Test.Cli.main ~name:"contentstore_store_namespace_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
