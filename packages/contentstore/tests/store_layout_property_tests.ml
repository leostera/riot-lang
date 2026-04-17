open Std
open Propane
module Test = Std.Test

let examples = 500

let property_config = { Property.default_config with test_count = examples }

let distinct_string_pair_arb =
  let gen =
    Generator.map
      (fun (left, right) ->
        if String.equal left right then
          (left, right ^ "\x00")
        else
          (left, right))
      (Generator.pair Generator.string Generator.string)
  in
  Arbitrary.make ~print:(Printer.pair Printer.string Printer.string) gen

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

let namespace = fun parts -> Contentstore.Namespace.from_parts parts |> Result.expect ~msg:"invalid test namespace"

let with_store = fun prefix parts fn ->
  Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let store = Contentstore.create
        ~root:Path.(tmpdir / Path.v "cache")
        ~ns:(namespace parts)
        ~policy:Contentstore.Policy.default in
      fn ~tmpdir ~store) |> Result.unwrap_or ~default:false

let object_path = fun store hash ->
  let hex = Crypto.Digest.hex hash in
  Path.(Contentstore.root store
  / Path.v "objects"
  / Path.v "objects"
  / Path.v (String.sub hex ~offset:0 ~len:2)
  / Path.v hex)

let named_object_path = fun store key ->
  let key_hash = Crypto.hash_string key |> Crypto.Digest.hex in
  Path.(Contentstore.root store
  / Path.v "named"
  / Path.v "objects"
  / Path.v (String.sub key_hash ~offset:0 ~len:2)
  / Path.v key_hash)

let hash_dir_is_deterministic =
  Property.for_all Arbitrary.string
    (fun content ->
      with_store "contentstore-prop-layout-stable" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let hash = Crypto.hash_string content in
          Path.equal (Contentstore.hash_dir_of store hash) (Contentstore.hash_dir_of store hash)))

let distinct_hashes_have_distinct_tree_paths =
  Property.for_all distinct_string_pair_arb
    (fun (left, right) ->
      with_store "contentstore-prop-layout-distinct" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let left_hash = Crypto.hash_string left in
          let right_hash = Crypto.hash_string right in
          not
            (Path.equal
              (Contentstore.hash_dir_of store left_hash)
              (Contentstore.hash_dir_of store right_hash))))

let layout_domains_are_disjoint =
  Property.for_all Arbitrary.(pair string string)
    (fun (content, key) ->
      with_store "contentstore-prop-layout-domains" [ "objects" ]
        (fun ~tmpdir:_ ~store ->
          let hash = Crypto.hash_string content in
          let tree_path = Contentstore.hash_dir_of store hash in
          let object_path = object_path store hash in
          let named_path = named_object_path store key in
          let temp_root = Path.(Contentstore.root store / Path.v "tmp") in
          not (Path.equal tree_path object_path)
          && not (Path.equal tree_path named_path)
          && not (Path.equal object_path named_path)
          && not (Path.equal tree_path temp_root)
          && not (Path.equal object_path temp_root)
          && not (Path.equal named_path temp_root)))

let tests = [
  Test.property
    "hash_dir_of is deterministic"
    ~examples
    (fun _ctx -> assert_property "hash_dir_of is deterministic" hash_dir_is_deterministic);
  Test.property
    "distinct hashes have distinct tree paths"
    ~examples
    (fun _ctx -> assert_property "distinct hashes have distinct tree paths" distinct_hashes_have_distinct_tree_paths);
  Test.property
    "layout domains are disjoint"
    ~examples
    (fun _ctx -> assert_property "layout domains are disjoint" layout_domains_are_disjoint);
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"contentstore_store_layout_property_tests" ~tests ~args)
    ~args:Env.args
    ()
