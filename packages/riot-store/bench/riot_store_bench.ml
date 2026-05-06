open Std
open Std.Bench

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let _ =
    Fs.create_dir_all parent
    |> Result.expect ~msg:"create bench parent should succeed"
  in
  Fs.write contents path
  |> Result.expect ~msg:"write bench file should succeed"

let payload = fun ~size ~seed ->
  String.init
    ~len:size
    ~fn:(fun index -> Char.from_int_unchecked (Char.to_int 'a' + ((index + seed) mod 26)))

let make_workspace = fun root ->
  Riot_model.Workspace.make ~root ~target_dir:(Path.v "target") ~packages:[] ()

let create_store = fun root ->
  let workspace = make_workspace root in
  Riot_store.Store.create ~workspace

let make_bench_save_single_output = fun root ~size ->
  let store_root = Path.(root / Path.v ("save-single-" ^ Int.to_string size)) in
  let store = create_store store_root in
  let sandbox = Path.(store_root / Path.v "sandbox") in
  let output = Path.(sandbox / Path.v "pkg.cmx") in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    write_file output (payload ~size ~seed:iteration);
    let hash = Crypto.hash_string ("riot-store:save-single:" ^ Int.to_string iteration) in
    let _ =
      Riot_store.Store.save
        store
        ~package:"pkg"
        ~input_hash:hash
        ~sandbox_dir:sandbox
        ~outs:[ output ]
      |> Result.expect ~msg:"save single output bench should succeed"
    in
    ()

let make_bench_save_nested_outputs = fun root ~size ->
  let store_root = Path.(root / Path.v ("save-nested-" ^ Int.to_string size)) in
  let store = create_store store_root in
  let sandbox = Path.(store_root / Path.v "sandbox") in
  let outputs = [
    Path.(sandbox / Path.v "lib" / Path.v "pkg.cmxa");
    Path.(sandbox / Path.v "lib" / Path.v "pkg.a");
    Path.(sandbox / Path.v "bin" / Path.v "pkg-tool");
  ]
  in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    List.for_each
      outputs
      ~fn:(fun path ->
        let name = Path.basename path in
        write_file path (payload ~size ~seed:(iteration + String.length name)));
    let hash = Crypto.hash_string ("riot-store:save-nested:" ^ Int.to_string iteration) in
    let _ =
      Riot_store.Store.save store ~package:"pkg" ~input_hash:hash ~sandbox_dir:sandbox ~outs:outputs
      |> Result.expect ~msg:"save nested outputs bench should succeed"
    in
    ()

let prepare_promote_fixture = fun root ->
  let store_root = Path.(root / Path.v "promote") in
  let store = create_store store_root in
  let sandbox = Path.(store_root / Path.v "sandbox") in
  let outputs = [
    Path.(sandbox / Path.v "lib" / Path.v "pkg.cmxa");
    Path.(sandbox / Path.v "lib" / Path.v "pkg.a");
    Path.(sandbox / Path.v "bin" / Path.v "pkg-tool");
  ]
  in
  List.for_each
    (List.enumerate outputs)
    ~fn:(fun (index, path) -> write_file path (payload ~size:(1_024 + (index * 256)) ~seed:index));
  let hash = Crypto.hash_string "riot-store:promote-fixture" in
  let _ =
    Riot_store.Store.save store ~package:"pkg" ~input_hash:hash ~sandbox_dir:sandbox ~outs:outputs
    |> Result.expect ~msg:"prepare promote fixture should succeed"
  in
  (store, hash, store_root)

let make_bench_promote_nested_outputs = fun root ->
  let (store, hash, store_root) = prepare_promote_fixture root in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let target_dir = Path.(store_root / Path.v ("promoted-" ^ Int.to_string iteration)) in
    let _ =
      Riot_store.Store.promote store hash ~target_dir
      |> Result.expect ~msg:"promote bench should succeed"
    in
    ()

let prepare_export_fixture = fun root ->
  let store_root = Path.(root / Path.v "exports") in
  let store = create_store store_root in
  let sandbox = Path.(store_root / Path.v "sandbox") in
  let output = Path.(sandbox / Path.v "lib" / Path.v "pkg.cmxa") in
  write_file output (payload ~size:4_096 ~seed:0);
  let hash = Crypto.hash_string "riot-store:export-fixture" in
  let exports = [
    Riot_store.Store.{
      name = "pkg.cmxa";
      path = Path.v "lib/pkg.cmxa";
      action_hash = Crypto.Digest.hex hash;
    };
  ]
  in
  let _ =
    Riot_store.Store.save
      store
      ~package:"pkg"
      ~input_hash:hash
      ~exports
      ~sandbox_dir:sandbox
      ~outs:[ output ]
    |> Result.expect ~msg:"prepare export fixture should succeed"
  in
  (store, exports, store_root)

let make_bench_materialize_exports = fun root ->
  let (store, exports, store_root) = prepare_export_fixture root in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let target_dir = Path.(store_root / Path.v ("exports-out-" ^ Int.to_string iteration)) in
    let _ =
      Riot_store.Store.materialize_package_exports store ~exports ~target_dir
      |> Result.expect ~msg:"materialize exports bench should succeed"
    in
    ()

let make_bench_save_plan_bundle = fun root ->
  let store_root = Path.(root / Path.v "save-plan") in
  let store = create_store store_root in
  let plan = Data.Json.Object [
    ("version", Data.Json.Int 1);
    ("package", Data.Json.String "pkg");
    ("module_graph", Data.Json.Object [ ("nodes", Data.Json.Array []); ]);
    ("action_graph", Data.Json.Object [ ("nodes", Data.Json.Array []); ]);
  ]
  in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let hash = Crypto.hash_string ("riot-store:plan-save:" ^ Int.to_string iteration) in
    let _ =
      Riot_store.Store.save_plan_bundle store ~hash ~plan
      |> Result.expect ~msg:"save plan bundle bench should succeed"
    in
    ()

let make_bench_load_plan_bundle = fun root ->
  let store_root = Path.(root / Path.v "load-plan") in
  let store = create_store store_root in
  let hash = Crypto.hash_string "riot-store:plan-load" in
  let plan = Data.Json.Object [
    ("version", Data.Json.Int 1);
    ("package", Data.Json.String "pkg");
    ("module_graph", Data.Json.Object [ ("nodes", Data.Json.Array []); ]);
    ("action_graph", Data.Json.Object [ ("nodes", Data.Json.Array []); ]);
  ]
  in
  let _ =
    Riot_store.Store.save_plan_bundle store ~hash ~plan
    |> Result.expect ~msg:"prepare load plan bundle bench should succeed"
  in
  fun () ->
    match Riot_store.Store.load_plan_bundle store ~hash with
    | Some _ -> ()
    | None -> panic "load plan bundle bench expected cached plan"

let fast_config: Bench.bench_config = { iterations = 120; warmup = 12 }

let medium_config: Bench.bench_config = { iterations = 60; warmup = 8 }

let heavy_config: Bench.bench_config = { iterations = 24; warmup = 4 }

let benchmark_suite = fun root ->
  Bench.[
    with_config
      ~config:medium_config
      "riot-store save single output miss 4kb"
      (make_bench_save_single_output root ~size:4_096);
    with_config
      ~config:heavy_config
      "riot-store save nested outputs miss 3x4kb"
      (make_bench_save_nested_outputs root ~size:4_096);
    with_config
      ~config:medium_config
      "riot-store promote nested outputs"
      (make_bench_promote_nested_outputs root);
    with_config
      ~config:medium_config
      "riot-store materialize package exports"
      (make_bench_materialize_exports root);
    with_config ~config:fast_config "riot-store save plan bundle" (make_bench_save_plan_bundle root);
    with_config ~config:fast_config "riot-store load plan bundle" (make_bench_load_plan_bundle root);
  ]

let main ~args =
  match Fs.with_tempdir
    ~prefix:"riot_store_bench"
    (fun root ->
      Bench.Cli.main
        ~name:"riot-store benchmarks"
        ~benchmarks:(benchmark_suite root)
        ~args) with
  | Ok result -> result
  | Error err -> panic ("failed to prepare riot-store bench fixture: " ^ IO.error_message err)

let () = Runtime.run ~main ~args:Env.args ()
