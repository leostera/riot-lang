open Std
open Propane

module Test = Std.Test

let examples = 100

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

let namespace = fun parts ->
  Contentstore.Namespace.from_parts parts
  |> Result.expect ~msg:"invalid test namespace"

let trim_list = fun limit items ->
  let rec loop remaining acc rest =
    if remaining = 0 then
      List.reverse acc
    else
      match rest with
      | [] -> List.reverse acc
      | item :: tail -> loop (remaining - 1) (item :: acc) tail
  in
  loop limit [] items

let update_named_expectation = fun expected key content ->
  let rec loop acc remaining =
    match remaining with
    | [] -> List.reverse ((key, content) :: acc)
    | (existing_key, _) :: rest when String.equal existing_key key ->
        List.append (List.reverse acc) ((key, content) :: rest)
    | entry :: rest -> loop (entry :: acc) rest
  in
  loop [] expected

let save_object_entries = fun store contents ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.reverse acc)
    | (index, content) :: rest ->
        let hash = Crypto.hash_string ("object:" ^ Int.to_string index ^ ":" ^ content) in
        match Contentstore.save_object store ~hash ~content with
        | Ok () -> loop ((hash, content) :: acc) rest
        | Error _ -> Error ()
  in
  loop [] (List.enumerate (trim_list 12 contents))

let save_named_entries = fun store contents ->
  let rec loop expected entries =
    match entries with
    | [] -> Ok expected
    | (index, content) :: rest ->
        let key = "key-" ^ Int.to_string (index mod 4) in
        match Contentstore.save_named_object store ~key ~content with
        | Ok () -> loop (update_named_expectation expected key content) rest
        | Error _ -> Error ()
  in
  loop [] (List.enumerate (trim_list 12 contents))

let save_tree_entries = fun tmpdir store contents ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.reverse acc)
    | (index, content) :: rest ->
        let hash = Crypto.hash_string ("tree:" ^ Int.to_string index ^ ":" ^ content) in
        let source_dir = Path.(tmpdir / Path.v ("tree-" ^ Int.to_string index)) in
        let payload = Path.(source_dir / Path.v "payload.txt") in
        let _ =
          Fs.create_dir_all source_dir
          |> Result.expect ~msg:"create source dir should succeed"
        in
        let _ =
          Fs.write content payload
          |> Result.expect ~msg:"write tree payload should succeed"
        in
        match Contentstore.commit_dir store ~hash ~source_dir with
        | Ok () -> loop ((hash, content) :: acc) rest
        | Error _ -> Error ()
  in
  loop [] (List.enumerate (trim_list 8 contents))

let verify_object_entries = fun store expected ->
  let rec loop entries =
    match entries with
    | [] -> true
    | (hash, content) :: rest ->
        match Contentstore.open_object store ~hash
        |> Result.map ~fn:read_opened_file with
        | Ok loaded when String.equal loaded content -> loop rest
        | _ -> false
  in
  loop expected

let verify_named_entries = fun store expected ->
  let rec loop entries =
    match entries with
    | [] -> true
    | (key, content) :: rest ->
        match Contentstore.open_named_object store ~key
        |> Result.map ~fn:read_opened_file with
        | Ok loaded when String.equal loaded content -> loop rest
        | _ -> false
  in
  loop expected

let verify_tree_entries = fun store expected ->
  let rec loop entries =
    match entries with
    | [] -> true
    | (hash, content) :: rest ->
        match Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / Path.v "payload.txt") with
        | Ok loaded when String.equal loaded content -> loop rest
        | _ -> false
  in
  loop expected

let scope_is_empty = fun store scope ->
  let dir = Path.(Contentstore.root store / Path.v "tmp" / Path.v scope) in
  match Fs.read_dir dir with
  | Ok entries -> List.is_empty (Iter.MutIterator.to_list entries)
  | Error _ -> true

let run_workload = fun ~tmpdir ~store ~object_contents ~named_contents ~tree_contents ->
  match save_object_entries store object_contents with
  | Error () -> Error ()
  | Ok objects ->
      match save_named_entries store named_contents with
      | Error () -> Error ()
      | Ok named ->
          match save_tree_entries tmpdir store tree_contents with
          | Error () -> Error ()
          | Ok trees -> Ok (objects, named, trees)

let test_commit_order_preserves_observable_objects = fun _ctx ->
  Fs.with_tempdir
    ~prefix:"contentstore-prop-commit-order"
    (fun tmpdir ->
      let entries = [ (0, "payload-1"); (1, "payload-2"); (2, "payload-3"); (3, "payload-4"); ] in
      let left =
        Contentstore.create
          ~root:Path.(tmpdir / Path.v "left-cache")
          ~ns:(namespace [ "workload" ])
          ~policy:Contentstore.Policy.default
      in
      let right =
        Contentstore.create
          ~root:Path.(tmpdir / Path.v "right-cache")
          ~ns:(namespace [ "workload" ])
          ~policy:Contentstore.Policy.default
      in
      let rec save_entries store pending =
        match pending with
        | [] -> Ok ()
        | (index, content) :: rest ->
            let hash = Crypto.hash_string ("object:" ^ Int.to_string index ^ ":" ^ content) in
            match Contentstore.save_object store ~hash ~content with
            | Ok () -> save_entries store rest
            | Error err -> Error (Contentstore.Store.error_message err)
      in
      let rec verify_entries pending =
        match pending with
        | [] -> Ok ()
        | (index, content) :: rest ->
            let hash = Crypto.hash_string ("object:" ^ Int.to_string index ^ ":" ^ content) in
            let verify_one store =
              match Contentstore.open_object store ~hash
              |> Result.map ~fn:read_opened_file with
              | Ok loaded when String.equal loaded content -> Ok ()
              | Ok _ -> Error "expected commit order to preserve observable object contents"
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            match verify_one left with
            | Error _ as err -> err
            | Ok () ->
                match verify_one right with
                | Error _ as err -> err
                | Ok () -> verify_entries rest
      in
      match save_entries left entries with
      | Error _ as err -> err
      | Ok () ->
          match save_entries right (List.reverse entries) with
          | Error _ as err -> err
          | Ok () -> verify_entries entries)
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let mixed_workload_leaves_no_temp_files =
  Property.for_all
    Arbitrary.(triple (list string) (list string) (list string))
    (fun (object_contents, named_contents, tree_contents) ->
      Fs.with_tempdir
        ~prefix:"contentstore-prop-no-temp"
        (fun tmpdir ->
          let store =
            Contentstore.create
              ~root:Path.(tmpdir / Path.v "cache")
              ~ns:(namespace [ "workload" ])
              ~policy:Contentstore.Policy.default
          in
          match run_workload ~tmpdir ~store ~object_contents ~named_contents ~tree_contents with
          | Error () -> false
          | Ok _ ->
              scope_is_empty store "immutable"
              && scope_is_empty store "mutable"
              && scope_is_empty store "trees")
      |> Result.unwrap_or ~default:false)

let reopen_preserves_reachable_workload =
  Property.for_all
    Arbitrary.(triple (list string) (list string) (list string))
    (fun (object_contents, named_contents, tree_contents) ->
      Fs.with_tempdir
        ~prefix:"contentstore-prop-reopen-workload"
        (fun tmpdir ->
          let root = Path.(tmpdir / Path.v "cache") in
          let store =
            Contentstore.create
              ~root
              ~ns:(namespace [ "workload" ])
              ~policy:Contentstore.Policy.default
          in
          match run_workload ~tmpdir ~store ~object_contents ~named_contents ~tree_contents with
          | Error () -> false
          | Ok (objects, named, trees) ->
              let reopened =
                Contentstore.create
                  ~root
                  ~ns:(namespace [ "workload" ])
                  ~policy:Contentstore.Policy.default
              in
              verify_object_entries reopened objects
              && verify_named_entries reopened named
              && verify_tree_entries reopened trees)
      |> Result.unwrap_or ~default:false)

let tests = [
  Test.case
    ~size:Large
    "commit order preserves observable objects"
    test_commit_order_preserves_observable_objects;
  Test.property
    "mixed workload leaves no temp files"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property
        "mixed workload leaves no temp files"
        mixed_workload_leaves_no_temp_files);
  Test.property
    "reopen preserves reachable workload data"
    ~size:Large
    ~examples
    (fun _ctx ->
      assert_property
        "reopen preserves reachable workload data"
        reopen_preserves_reachable_workload);
]

let main ~args = Test.Cli.main ~name:"contentstore_store_workload_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
