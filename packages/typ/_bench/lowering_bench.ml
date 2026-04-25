open Std
module Typ = Typ

let bench_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let repeat_lowerings = 1_000

let repeat_hot_bundle = 250

let rec find_workspace_root = fun dir ->
  let kernel_new_dir = Path.join dir (Path.v "packages/kernel-new") in
  match Fs.is_dir kernel_new_dir with
  | Ok true -> dir
  | Ok false
  | Error _ -> (
      match Path.parent dir with
      | Some parent when not (Path.equal parent dir) -> find_workspace_root parent
      | _ -> panic
        (format
          Format.[
            str "expected benchmark to run inside the Riot workspace, starting from ";
            str (Path.to_string dir);
          ])
    )

let workspace_root = fun () ->
  Env.current_dir ()
  |> Result.expect ~msg:"expected current working directory for typ lowering bench"
  |> find_workspace_root

let expect_cst = fun ~filename parse_result ->
  match Syn.build_cst parse_result with
  | Ok cst -> cst
  | Error (Syn.Parse_diagnostics diagnostics) -> panic
    (format
      Format.[
        str "expected successful parse for ";
        str filename;
        str " but got diagnostics: ";
        str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
      ])
  | Error (Syn.Cst_builder_error error) -> panic
    (format
      Format.[
        str "expected successful CST for ";
        str filename;
        str " but CST build failed: ";
        str error.message;
      ])

let prepared_source_of_path = fun ~source_id path ->
  let text = Fs.read path
  |> Result.expect ~msg:("expected benchmark fixture at " ^ Path.to_string path) in
  let parse_result = Syn.parse ~filename:path text in
  let cst = expect_cst ~filename:(Path.to_string path) parse_result in
  let implicit_opens = [] in
  Typ.Model.Source.make_prepared
    ~source_id:(Typ.Model.SourceId.of_int source_id)
    ~kind:Typ.Model.Source.File
    ~module_name:(Typ.Model.Source.infer_module_name (Typ.Model.Source.Path path))
    ~implicit_opens
    ~origin:(Typ.Model.Source.Path path)
    ~revision:0
    ~source_hash:(Typ.Model.Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let hot_paths = fun workspace_root ->
  [
    Path.join workspace_root (Path.v "packages/kernel-new/src/error.ml");
    Path.join workspace_root (Path.v "packages/kernel-new/src/process/unix.ml");
    Path.join workspace_root (Path.v "packages/kernel-new/src/fs/file/file.ml");
    Path.join workspace_root (Path.v "packages/kernel-new/src/net/tcp_stream/unix.ml");
    Path.join workspace_root (Path.v "packages/kernel-new/src/net/tcp_listener/unix.ml");
  ]

let prepared_hot_sources = fun workspace_root ->
  hot_paths workspace_root
  |> List.mapi (fun index path -> prepared_source_of_path ~source_id:index path)

let lower_once = fun (source: Typ.Model.Source.t) ->
  let _semantic_tree = Typ.Lower.lower_source_file ~source source.cst in
  ()

let repeat = fun count f ->
  let rec loop remaining =
    if remaining <= 0 then
      ()
    else (
      f ();
      loop (remaining - 1)
    )
  in
  loop count

let bench_lower_source = fun label source ->
  Bench.with_config
    ~config:bench_config
    label
    (fun () -> repeat repeat_lowerings (fun () -> lower_once source))

let bench_lower_bundle = fun label sources ->
  Bench.with_config
    ~config:bench_config
    label
    (fun () -> repeat repeat_hot_bundle (fun () -> sources |> List.iter lower_once))

let benchmark_suite = fun () ->
  let workspace_root = workspace_root () in
  let prepared_hot_sources = prepared_hot_sources workspace_root in
  let source_by_path path =
    prepared_hot_sources
    |> List.find
      (fun (source: Typ.Model.Source.t) ->
        match source.origin with
        | Typ.Model.Source.Path source_path -> Path.equal source_path path
        | Typ.Model.Source.Label _ -> false)
  in
  Bench.[
    bench_lower_source
      "lower kernel-new/error.ml x1000"
      (source_by_path (Path.join workspace_root (Path.v "packages/kernel-new/src/error.ml")));
    bench_lower_source
      "lower kernel-new/process/unix.ml x1000"
      (source_by_path (Path.join workspace_root (Path.v "packages/kernel-new/src/process/unix.ml")));
    bench_lower_source
      "lower kernel-new/fs/file/file.ml x1000"
      (source_by_path (Path.join workspace_root (Path.v "packages/kernel-new/src/fs/file/file.ml")));
    bench_lower_source
      "lower kernel-new/net/tcp_stream/unix.ml x1000"
      (source_by_path
        (Path.join workspace_root (Path.v "packages/kernel-new/src/net/tcp_stream/unix.ml")));
    bench_lower_source
      "lower kernel-new/net/tcp_listener/unix.ml x1000"
      (source_by_path
        (Path.join workspace_root (Path.v "packages/kernel-new/src/net/tcp_listener/unix.ml")));
    bench_lower_bundle "lower kernel-new hot lowering bundle x250" prepared_hot_sources;
  ]

let main ~args = Bench.Cli.main ~name:"typ lowering benchmarks" ~benchmarks:(benchmark_suite ()) ~args

let () = Runtime.run ~main ~args:Env.args ()
