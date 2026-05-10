open Std
open Std.Bench

type Runtime.Message.t +=
  | Contentstore_bench_go
  | Contentstore_bench_done of (unit, string) result

let scratch_counter = Sync.Atomic.make 0

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
  |> Result.expect ~msg:"invalid bench namespace"

let temp_root = fun () ->
  match Env.get Env.String ~var:"TMPDIR" with
  | Some dir when dir != "" -> Path.v dir
  | _ ->
      match Env.get Env.String ~var:"TEMP" with
      | Some dir when dir != "" -> Path.v dir
      | _ ->
          match Env.get Env.String ~var:"TMP" with
          | Some dir when dir != "" -> Path.v dir
          | _ -> Path.v "/tmp"

let make_scratch_dir = fun prefix ->
  let pid =
    Process.id ()
    |> Int32.to_string
  in
  let nanos =
    Time.SystemTime.duration_since_epoch ()
    |> Time.Duration.to_nanos
    |> Int64.to_string
  in
  let counter =
    Sync.Atomic.fetch_and_add scratch_counter 1
    |> Int.to_string
  in
  let path =
    Path.(temp_root ()
    / Path.v "contentstore-bench"
    / Path.v (prefix ^ "-" ^ pid ^ "-" ^ nanos ^ "-" ^ counter))
  in
  let _ =
    Fs.create_dir_all path
    |> Result.expect ~msg:"create bench scratch dir should succeed"
  in
  path

let payload = fun ~size ~seed ->
  String.init
    ~len:size
    ~fn:(fun index -> Char.from_int_unchecked (Char.to_int 'a' + ((index + seed) mod 26)))

let create_store = fun tmpdir parts ->
  Contentstore.create
    ~root:Path.(tmpdir / Path.v "cache")
    ~ns:(namespace parts)
    ~policy:Contentstore.Policy.default

let write_tree = fun ~root ~file_count ~file_size ~seed ->
  let rec loop index =
    if index = file_count then
      Ok ()
    else
      let dir = Path.(root / Path.v ("bucket-" ^ Int.to_string (index mod 32))) in
      let path = Path.(dir / Path.v ("file-" ^ Int.to_string index ^ ".bin")) in
      let _ =
        Fs.create_dir_all dir
        |> Result.expect ~msg:"create tree dir should succeed"
      in
      let _ =
        Fs.write (payload ~size:file_size ~seed:(seed + index)) path
        |> Result.expect ~msg:"write tree file should succeed"
      in
      loop (index + 1)
  in
  loop 0

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(fun msg ->
        match msg with
        | Contentstore_bench_go -> Select ()
        | _ -> Skip)
      ~timeout:(Time.Duration.from_secs 5)
      ()
  in
  ()

let collect_worker_results = fun count ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      match receive
        ~selector:(fun msg ->
          match msg with
          | Contentstore_bench_done result -> Select result
          | _ -> Skip)
        ~timeout:(Time.Duration.from_secs 10)
        () with
      | Ok () -> loop (remaining - 1)
      | Error err -> Error err
  in
  loop count

let run_concurrent = fun ~workers ~worker ->
  let parent = self () in
  let rec spawn_all index acc =
    if index = workers then
      List.reverse acc
    else
      let pid =
        spawn
          (fun () ->
            wait_for_go ();
            send parent (Contentstore_bench_done (worker index));
            Ok ())
      in
      spawn_all (index + 1) (pid :: acc)
  in
  let pids = spawn_all 0 [] in
  List.for_each pids ~fn:(fun pid -> send pid Contentstore_bench_go);
  collect_worker_results workers

let make_bench_save_object_miss = fun ~size ->
  let tmpdir = make_scratch_dir ("save-object-miss-" ^ Int.to_string size) in
  let store = create_store tmpdir [ "objects" ] in
  let content = payload ~size ~seed:0 in
  let counter = ref 0 in
  fun () ->
    let hash = Crypto.hash_string ("object-miss:" ^ Int.to_string !counter) in
    counter := !counter + 1;
    let _ =
      Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"save_object miss should succeed"
    in
    ()

let make_bench_save_object_hit = fun ~size ->
  let tmpdir = make_scratch_dir ("save-object-hit-" ^ Int.to_string size) in
  let store = create_store tmpdir [ "objects" ] in
  let content = payload ~size ~seed:0 in
  let hash = Crypto.hash_string ("object-hit:" ^ Int.to_string size) in
  let _ =
    Contentstore.save_object store ~hash ~content
    |> Result.expect ~msg:"initial save_object should succeed"
  in
  fun () ->
    let _ =
      Contentstore.save_object store ~hash ~content
      |> Result.expect ~msg:"save_object hit should succeed"
    in
    ()

let make_bench_open_object_read = fun ~size ->
  let tmpdir = make_scratch_dir ("open-object-read-" ^ Int.to_string size) in
  let store = create_store tmpdir [ "objects" ] in
  let content = payload ~size ~seed:0 in
  let hash = Crypto.hash_string ("object-read:" ^ Int.to_string size) in
  let _ =
    Contentstore.save_object store ~hash ~content
    |> Result.expect ~msg:"save_object should succeed"
  in
  fun () ->
    let loaded =
      Contentstore.open_object store ~hash
      |> Result.map ~fn:read_opened_file
      |> Result.expect ~msg:"open_object should succeed"
    in
    let _ = String.length loaded in
    ()

let make_bench_save_named_object_overwrite = fun ~size ->
  let tmpdir = make_scratch_dir ("save-named-object-" ^ Int.to_string size) in
  let store = create_store tmpdir [ "named" ] in
  let counter = ref 0 in
  fun () ->
    let content = payload ~size ~seed:!counter in
    counter := !counter + 1;
    let _ =
      Contentstore.save_named_object store ~key:"current" ~content
      |> Result.expect ~msg:"save_named_object should succeed"
    in
    ()

let make_bench_save_file_miss = fun ~size ->
  let tmpdir = make_scratch_dir ("save-file-miss-" ^ Int.to_string size) in
  let store = create_store tmpdir [ "imports" ] in
  let source = Path.(tmpdir / Path.v "source.bin") in
  let _ =
    Fs.write (payload ~size ~seed:0) source
    |> Result.expect ~msg:"write source should succeed"
  in
  let counter = ref 0 in
  fun () ->
    let hash = Crypto.hash_string ("file-miss:" ^ Int.to_string !counter) in
    counter := !counter + 1;
    let _ =
      Contentstore.save_file store ~hash ~source
      |> Result.expect ~msg:"save_file should succeed"
    in
    ()

let make_bench_commit_dir = fun ~file_count ~file_size ->
  let tmpdir =
    make_scratch_dir ("commit-dir-" ^ Int.to_string file_count ^ "-" ^ Int.to_string file_size)
  in
  let store = create_store tmpdir [ "trees" ] in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    let source_dir = Path.(tmpdir / Path.v ("tree-" ^ Int.to_string iteration)) in
    let hash = Crypto.hash_string ("tree:" ^ Int.to_string iteration) in
    counter := iteration + 1;
    let _ =
      Fs.create_dir_all source_dir
      |> Result.expect ~msg:"create source dir should succeed"
    in
    let _ =
      write_tree ~root:source_dir ~file_count ~file_size ~seed:iteration
      |> Result.expect ~msg:"write_tree should succeed"
    in
    let _ =
      Contentstore.commit_dir store ~hash ~source_dir
      |> Result.expect ~msg:"commit_dir should succeed"
    in
    ()

let make_bench_same_key_overwrite_contention = fun ~workers ~size ->
  let tmpdir = make_scratch_dir ("named-contention-" ^ Int.to_string workers) in
  let store = create_store tmpdir [ "contended" ] in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let _ =
      run_concurrent
        ~workers
        ~worker:(fun index ->
          let content = payload ~size ~seed:(iteration + index) in
          Contentstore.save_named_object store ~key:"current" ~content
          |> Result.map_err ~fn:Contentstore.Store.error_message)
      |> Result.expect ~msg:"concurrent same-key overwrite should succeed"
    in
    ()

let make_bench_same_hash_object_contention = fun ~workers ~size ->
  let tmpdir = make_scratch_dir ("object-contention-" ^ Int.to_string workers) in
  let store = create_store tmpdir [ "contended" ] in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    let hash = Crypto.hash_string ("contended-object:" ^ Int.to_string iteration) in
    counter := iteration + 1;
    let _ =
      run_concurrent
        ~workers
        ~worker:(fun index ->
          let content = payload ~size ~seed:(iteration + index) in
          Contentstore.save_object store ~hash ~content
          |> Result.map_err ~fn:Contentstore.Store.error_message)
      |> Result.expect ~msg:"concurrent same-hash object writes should succeed"
    in
    ()

let small_config = { iterations = 300; warmup = 30 }

let medium_config = { iterations = 120; warmup = 20 }

let large_config = { iterations = 40; warmup = 8 }

let xlarge_config = { iterations = 12; warmup = 3 }

let contention_config = { iterations = 30; warmup = 5 }

let heavy_contention_config = { iterations = 12; warmup = 3 }

let tree_small_config = { iterations = 25; warmup = 4 }

let tree_medium_config = { iterations = 10; warmup = 2 }

let tree_large_config = { iterations = 4; warmup = 1 }

let benchmarks =
  Bench.[
    with_config
      ~config:small_config
      "contentstore save_object miss 64b"
      (make_bench_save_object_miss ~size:64);
    with_config
      ~config:medium_config
      "contentstore save_object miss 4kb"
      (make_bench_save_object_miss ~size:4_096);
    with_config
      ~config:large_config
      "contentstore save_object miss 256kb"
      (make_bench_save_object_miss ~size:262_144);
    with_config
      ~config:xlarge_config
      "contentstore save_object miss 1mb"
      (make_bench_save_object_miss ~size:1_048_576);
    with_config
      ~config:medium_config
      "contentstore save_object hit 4kb"
      (make_bench_save_object_hit ~size:4_096);
    with_config
      ~config:small_config
      "contentstore open_object read 64b"
      (make_bench_open_object_read ~size:64);
    with_config
      ~config:medium_config
      "contentstore open_object read 4kb"
      (make_bench_open_object_read ~size:4_096);
    with_config
      ~config:large_config
      "contentstore open_object read 256kb"
      (make_bench_open_object_read ~size:262_144);
    with_config
      ~config:xlarge_config
      "contentstore open_object read 1mb"
      (make_bench_open_object_read ~size:1_048_576);
    with_config
      ~config:large_config
      "contentstore save_named_object overwrite 1kb"
      (make_bench_save_named_object_overwrite ~size:1_024);
    with_config
      ~config:large_config
      "contentstore save_file miss 256kb"
      (make_bench_save_file_miss ~size:262_144);
    with_config
      ~config:contention_config
      "contentstore same-key overwrite contention 4 writers"
      (make_bench_same_key_overwrite_contention ~workers:4 ~size:1_024);
    with_config
      ~config:heavy_contention_config
      "contentstore same-key overwrite contention 8 writers"
      (make_bench_same_key_overwrite_contention ~workers:8 ~size:1_024);
    with_config
      ~config:contention_config
      "contentstore same-hash object contention 4 writers"
      (make_bench_same_hash_object_contention ~workers:4 ~size:1_024);
    with_config
      ~config:heavy_contention_config
      "contentstore same-hash object contention 8 writers"
      (make_bench_same_hash_object_contention ~workers:8 ~size:1_024);
    with_config
      ~config:tree_small_config
      "contentstore commit_dir 1 file 256kb"
      (make_bench_commit_dir ~file_count:1 ~file_size:262_144);
    with_config
      ~config:tree_medium_config
      "contentstore commit_dir 100 files 2kb"
      (make_bench_commit_dir ~file_count:100 ~file_size:2_048);
    with_config
      ~config:tree_large_config
      "contentstore commit_dir 1000 files 256b"
      (make_bench_commit_dir ~file_count:1_000 ~file_size:256);
  ]

let main ~args = Bench.Cli.main ~name:"contentstore benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
