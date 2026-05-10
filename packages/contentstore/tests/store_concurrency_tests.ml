open Std

module Test = Std.Test

type Runtime.Message.t +=
  | Contentstore_test_go
  | Contentstore_worker_done of (unit, string) result
  | Contentstore_reader_done of (unit, string) result

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

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(fun msg ->
        match msg with
        | Contentstore_test_go -> Select ()
        | _ -> Skip)
      ~timeout:(Time.Duration.from_secs 5)
      ()
  in
  ()

let collect_results = fun count ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      match receive
        ~selector:(fun msg ->
          match msg with
          | Contentstore_worker_done result -> Select result
          | Contentstore_reader_done result -> Select result
          | _ -> Skip)
        ~timeout:(Time.Duration.from_secs 10)
        () with
      | Ok () -> loop (remaining - 1)
      | Error err -> Error err
  in
  loop count

let test_concurrent_same_hash_object_writers_converge = fun _ctx ->
  with_store
    "contentstore-concurrent-same-hash"
    [ "concurrent" ]
    (fun ~tmpdir:_ ~store ->
      let hash = Crypto.hash_string "shared-hash" in
      let parent = self () in
      let left = "left-payload" in
      let right = "right-payload" in
      let worker content =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.save_object store ~hash ~content with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let left_pid = worker left in
      let right_pid = worker right in
      send left_pid Contentstore_test_go;
      send right_pid Contentstore_test_go;
      match collect_results 2 with
      | Error err -> Error err
      | Ok () ->
          match Contentstore.open_object store ~hash
          |> Result.map ~fn:read_opened_file with
          | Ok loaded when String.equal loaded left || String.equal loaded right -> Ok ()
          | Ok _ -> Error "expected concurrent same-hash writers to converge to one complete object"
          | Error err -> Error (Contentstore.Store.error_message err))

let test_concurrent_same_key_named_writers_converge = fun _ctx ->
  with_store
    "contentstore-concurrent-same-key"
    [ "concurrent" ]
    (fun ~tmpdir:_ ~store ->
      let key = "current" in
      let parent = self () in
      let left = "left-named-payload" in
      let right = "right-named-payload" in
      let worker content =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.save_named_object store ~key ~content with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let left_pid = worker left in
      let right_pid = worker right in
      send left_pid Contentstore_test_go;
      send right_pid Contentstore_test_go;
      match collect_results 2 with
      | Error err -> Error err
      | Ok () ->
          match Contentstore.open_named_object store ~key
          |> Result.map ~fn:read_opened_file with
          | Ok loaded when String.equal loaded left || String.equal loaded right -> Ok ()
          | Ok _ ->
              Error "expected concurrent same-key writers to converge to one complete named object"
          | Error err -> Error (Contentstore.Store.error_message err))

let test_concurrent_same_hash_commit_dir_writers_converge = fun _ctx ->
  with_store
    "contentstore-concurrent-commit-dir"
    [ "concurrent" ]
    (fun ~tmpdir ~store ->
      let hash = Crypto.hash_string "shared-tree" in
      let parent = self () in
      let make_source name payload =
        let source_dir = Path.(tmpdir / Path.v name) in
        let _ =
          Fs.create_dir_all source_dir
          |> Result.expect ~msg:"create source dir should succeed"
        in
        let _ =
          Fs.write payload Path.(source_dir / Path.v "payload.txt")
          |> Result.expect ~msg:"write payload should succeed"
        in
        source_dir
      in
      let left_dir = make_source "left" "left-tree" in
      let right_dir = make_source "right" "right-tree" in
      let worker source_dir =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.commit_dir store ~hash ~source_dir with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let left_pid = worker left_dir in
      let right_pid = worker right_dir in
      send left_pid Contentstore_test_go;
      send right_pid Contentstore_test_go;
      match collect_results 2 with
      | Error err -> Error err
      | Ok () ->
          match Fs.read_to_string Path.(Contentstore.hash_dir_of store hash / Path.v "payload.txt") with
          | Ok loaded when String.equal loaded "left-tree" || String.equal loaded "right-tree" ->
              Ok ()
          | Ok _ ->
              Error "expected concurrent same-hash commit_dir writers to converge to one complete tree"
          | Error _ -> Error "expected committed tree to stay readable after concurrent commit_dir")

let test_readers_see_old_or_new_named_values_during_overwrite = fun _ctx ->
  with_store
    "contentstore-concurrent-reader-writer"
    [ "concurrent" ]
    (fun ~tmpdir:_ ~store ->
      let parent = self () in
      let key = "current" in
      let left = "left-value!" in
      let right = "right-value" in
      let done_writing = ref false in
      let _ =
        Contentstore.save_named_object store ~key ~content:left
        |> Result.expect ~msg:"initial save_named_object should succeed"
      in
      let _writer =
        spawn
          (fun () ->
            wait_for_go ();
            let rec loop remaining current =
              if remaining = 0 then
                Ok ()
              else
                let next =
                  if String.equal current left then
                    right
                  else
                    left
                in
                match Contentstore.save_named_object store ~key ~content:next with
                | Ok () ->
                    yield ();
                    loop (remaining - 1) next
                | Error err -> Error (Contentstore.Store.error_message err)
            in
            let result = loop 100 left in
            done_writing := true;
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let reader_pid =
        spawn
          (fun () ->
            wait_for_go ();
            let rec loop remaining =
              if remaining = 0 then
                Ok ()
              else
                match Contentstore.open_named_object store ~key
                |> Result.map ~fn:read_opened_file with
                | Ok value when String.equal value left || String.equal value right ->
                    if !done_writing then
                      Ok ()
                    else (
                      yield ();
                      loop (remaining - 1)
                    )
                | Ok value ->
                    Error ("reader observed a partial named value during overwrite: "
                    ^ String.escaped value)
                | Error (Contentstore.Store.Missing _) ->
                    Error "reader observed a missing named value during overwrite"
                | Error err -> Error (Contentstore.Store.error_message err)
            in
            let result = loop 1_000 in
            send parent (Contentstore_reader_done result);
            Ok ())
      in
      send _writer Contentstore_test_go;
      send reader_pid Contentstore_test_go;
      collect_results 2)

let test_mixed_workload_does_not_cross_corrupt = fun _ctx ->
  with_store
    "contentstore-concurrent-mixed-workload"
    [ "concurrent" ]
    (fun ~tmpdir ~store ->
      let parent = self () in
      let object_hash = Crypto.hash_string "object" in
      let tree_hash = Crypto.hash_string "tree" in
      let source_dir = Path.(tmpdir / Path.v "tree") in
      let _ =
        Fs.create_dir_all source_dir
        |> Result.expect ~msg:"create source dir should succeed"
      in
      let _ =
        Fs.write "tree" Path.(source_dir / Path.v "payload.txt")
        |> Result.expect ~msg:"write tree payload should succeed"
      in
      let object_pid =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.save_object store ~hash:object_hash ~content:"object" with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let named_pid =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.save_named_object store ~key:"current" ~content:"named" with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      let tree_pid =
        spawn
          (fun () ->
            wait_for_go ();
            let result =
              match Contentstore.commit_dir store ~hash:tree_hash ~source_dir with
              | Ok () -> Ok ()
              | Error err -> Error (Contentstore.Store.error_message err)
            in
            send parent (Contentstore_worker_done result);
            Ok ())
      in
      send object_pid Contentstore_test_go;
      send named_pid Contentstore_test_go;
      send tree_pid Contentstore_test_go;
      match collect_results 3 with
      | Error err -> Error err
      | Ok () ->
          match (
            Contentstore.open_object store ~hash:object_hash
            |> Result.map ~fn:read_opened_file,
            Contentstore.open_named_object store ~key:"current"
            |> Result.map ~fn:read_opened_file,
            Fs.read_to_string Path.(Contentstore.hash_dir_of store tree_hash / Path.v "payload.txt")
          ) with
          | (Ok "object", Ok "named", Ok "tree") -> Ok ()
          | _ -> Error "expected mixed object/named/tree workload to stay isolated")

let tests = [
  Test.case
    "concurrent same-hash object writers converge"
    test_concurrent_same_hash_object_writers_converge;
  Test.case
    "concurrent same-key named writers converge"
    test_concurrent_same_key_named_writers_converge;
  Test.case
    "concurrent same-hash commit_dir writers converge"
    test_concurrent_same_hash_commit_dir_writers_converge;
  Test.case
    "readers see old or new named values during overwrite"
    test_readers_see_old_or_new_named_values_during_overwrite;
  Test.case "mixed workloads do not cross-corrupt" test_mixed_workload_does_not_cross_corrupt;
]

let main ~args = Test.Cli.main ~name:"contentstore_store_concurrency_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
