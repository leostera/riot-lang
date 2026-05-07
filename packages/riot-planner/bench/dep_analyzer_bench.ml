open Std
open Std.Bench
open Std.Collections

module Dep_analyzer = Riot_planner.Dep_analyzer
module Ir = Dep_analyzer.Ir
module Item = Ir.Item

type fixture = {
  path: Path.t;
  source_hash: Crypto.hash;
  parse_result: Syn.Parser.parse_result;
}

let checksum = ref 0

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create dep analyzer benchmark source slice"

let load_fixture = fun path ->
  let source =
    Fs.read path
    |> Result.expect ~msg:("failed to read dep analyzer benchmark fixture: " ^ Path.to_string path)
  in
  {
    path;
    source_hash = Crypto.hash_string source;
    parse_result = Syn.parse ~filename:path (source_slice source);
  }

let is_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let load_fixture_corpus = fun () ->
  let fixtures = Vector.with_capacity ~size:64 in
  Fs.Walker.walk
    ~roots:[ Path.v "packages/riot-planner/tests/deps_fixtures" ]
    ~f:(fun item ->
      let path = Fs.Walker.FileItem.path item in
      if is_source_file path then
        Vector.push fixtures ~value:(load_fixture path);
      Fs.Walker.Continue)
    ()
  |> Result.expect ~msg:"failed to walk dep analyzer benchmark fixture corpus";
  Vector.to_array fixtures
  |> Array.to_list

let rec item_weight = fun (item: Item.t) ->
  let items_weight = fun items ->
    List.fold_left items ~init:0 ~fn:(fun total item -> total + item_weight item)
  in
  match item with
  | Item.Use path -> List.length path
  | Item.Open body -> 1 + item_weight body
  | Item.Include (_, body) -> 1 + item_weight body
  | Item.Module { name; signature; body } ->
      String.length name + items_weight signature + items_weight body
  | Item.ModuleAlias { name; target } -> String.length name + item_weight target
  | Item.Functor { name; args; body } ->
      String.length name
      + List.fold_left
        args
        ~init:0
        ~fn:(fun total (arg: Item.functor_arg) ->
          let name_weight =
            match arg.name with
            | Some name -> String.length name
            | None -> 0
          in
          total + name_weight + items_weight arg.ascription)
      + items_weight body
  | Item.ModuleType { name; body } -> String.length name + items_weight body
  | Item.FunctorApply { callee; argument } -> item_weight callee + item_weight argument
  | Item.Constraint { expr; signature } -> item_weight expr + items_weight signature
  | Item.Typeof body -> item_weight body
  | Item.WithConstraint { base; constraints } -> item_weight base + items_weight constraints
  | Item.BindModules { modules; scope } ->
      List.fold_left
        modules
        ~init:0
        ~fn:(fun total (module_: Item.bound_module) ->
          total + String.length module_.name + items_weight module_.ascription)
      + items_weight scope
  | Item.Scope body -> items_weight body

let touch_summary = fun (summary: Ir.source_summary) ->
  checksum :=
    !checksum
    lxor
    List.fold_left summary.items ~init:0 ~fn:(fun total item -> total + item_weight item)

let analyze_fixture = fun fixture ->
  match Ir.analyze
    ~source:fixture.path
    ~source_hash:fixture.source_hash
    fixture.parse_result with
  | Ok summary -> touch_summary summary
  | Error (Ir.Parse_diagnostics diagnostics) ->
      panic ("dep analyzer IR benchmark parse diagnostics for "
      ^ Path.to_string fixture.path
      ^ ": "
      ^ String.concat "; " (List.map diagnostics ~fn:Syn.Diagnostic.to_string))

let analyze_corpus = fun fixtures ->
  List.for_each fixtures ~fn:analyze_fixture

let large_config: Bench.bench_config = { iterations = 800; warmup = 80 }

let corpus_config: Bench.bench_config = { iterations = 120; warmup = 20 }

let fixture_benchmark = fun ~name path ->
  let fixture = load_fixture path in
  Bench.with_config
    ~config:large_config
    ("dep_analyzer.ir " ^ name)
    (fun () -> analyze_fixture fixture)

let benchmarks = fun () ->
  let corpus = load_fixture_corpus () in
  [
    fixture_benchmark
      ~name:"vendored makedepend"
      (Path.v "packages/riot-planner/tests/deps_fixtures/0027_vendored_makedepend.ml");
    fixture_benchmark
      ~name:"tty"
      (Path.v "packages/riot-planner/tests/deps_fixtures/0028_tty.ml");
    fixture_benchmark
      ~name:"kernel mutiterator"
      (Path.v "packages/riot-planner/tests/deps_fixtures/0033_kernel_mutiterator.ml");
    fixture_benchmark
      ~name:"liveview counter"
      (Path.v "packages/riot-planner/tests/deps_fixtures/0031_liveview_counter.ml");
    fixture_benchmark
      ~name:"std compress gzip interface"
      (Path.v "packages/riot-planner/tests/deps_fixtures/0034_std_compress_gzip.mli");
    Bench.with_config
      ~config:corpus_config
      ("dep_analyzer.ir fixture corpus (" ^ Int.to_string (List.length corpus) ^ " files)")
      (fun () -> analyze_corpus corpus);
  ]

let main ~args =
  let result = Bench.Cli.main ~name:"dep-analyzer" ~benchmarks:(benchmarks ()) ~args in
  if !checksum = Int.min_int then
    panic "unreachable dep analyzer benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
