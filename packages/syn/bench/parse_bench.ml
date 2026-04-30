open Std
open Std.Collections
open Syn

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let make_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create parse benchmark source slice"

let load_fixture = fun name path ->
  let source =
    Fs.read path
    |> Result.expect ~msg:("failed to read parse benchmark fixture: " ^ Path.to_string path)
  in
  {
    name;
    path;
    source;
    slice = make_slice source;
  }

let touch_parse = fun (result: Parser.parse_result) ->
  let root = SyntaxTree.root result.Parser.tree in
  checksum := !checksum lxor root.SyntaxTree.full_width lxor Vector.length result.Parser.diagnostics

let bench_parse = fun fixture ->
  let result = parse ~filename:fixture.path fixture.slice in
  touch_parse result

let is_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let has_lossless_snapshot = fun path ->
  let snapshot_path =
    Path.to_string path ^ ".expected_lossless.json"
    |> Path.from_string
    |> Result.expect ~msg:"lossless snapshot path should stay valid UTF-8"
  in
  Fs.exists snapshot_path
  |> Result.unwrap_or ~default:false

let valid_fixture_skips = [
  "ocaml_docstrings.ml";
  "ocaml_extensions.ml";
  "ocaml_shortcut_ext_attr.ml";
]

let is_valid_fixture = fun path ->
  let basename = Path.basename path in
  not (List.any valid_fixture_skips ~fn:(fun name -> String.equal basename name))

let load_valid_fixture_corpus = fun () ->
  let fixtures = Vector.with_capacity ~size:1_100 in
  Fs.Walker.walk
    ~roots:[ Path.v "packages/syn/tests/fixtures" ]
    ~sort:true
    ~f:(fun item ->
      let path = Fs.Walker.FileItem.path item in
      (
        if is_source_file path && is_valid_fixture path && has_lossless_snapshot path then
          let name = Fs.Walker.FileItem.name item in
          Vector.push fixtures ~value:(load_fixture name path)
      );
      Fs.Walker.Continue)
    ()
  |> Result.expect ~msg:"failed to walk syn fixture corpus";
  fixtures

let bench_parse_corpus = fun fixtures ->
  let rec loop index =
    if index < Vector.length fixtures then (
      bench_parse (Vector.get_unchecked fixtures ~at:index);
      loop (index + 1)
    )
  in
  loop 0

let tiny_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 500; warmup = 50 }

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

let large_config: Bench.bench_config = { iterations = 12; warmup = 2 }

let corpus_config: Bench.bench_config = { iterations = 3; warmup = 1 }

let fixture_benchmark = fun ~config fixture ->
  Bench.with_config
    ~config
    ("parse: " ^ fixture.name)
    (fun () -> bench_parse fixture)

let selected_benchmarks = fun () -> [
  fixture_benchmark
    ~config:tiny_config
    (load_fixture "tiny let binding" (Path.v "packages/syn/tests/fixtures/0001_basic.ml"));
  fixture_benchmark
    ~config:small_config
    (load_fixture
      "quoted extensions"
      (Path.v "packages/syn/tests/fixtures/ocaml_quotedextensions.ml"));
  fixture_benchmark
    ~config:medium_config
    (load_fixture
      "docstrings"
      (Path.v "packages/syn/tests/fixtures/9104_comments_docstrings_bridge.ml"));
  fixture_benchmark
    ~config:large_config
    (load_fixture
      "vendored makedepend"
      (Path.v "packages/syn/tests/deps_fixtures/0027_vendored_makedepend.ml"));
]

let corpus_benchmark = fun () ->
  let fixtures = load_valid_fixture_corpus () in
  Bench.with_config
    ~config:corpus_config
    ("parse: valid fixture corpus (" ^ Int.to_string (Vector.length fixtures) ^ " files)")
    (fun () -> bench_parse_corpus fixtures)

let benchmarks = fun () -> selected_benchmarks () @ [ corpus_benchmark () ]

let main ~args =
  let result = Bench.Cli.main ~name:"syn parse" ~benchmarks:(benchmarks ()) ~args in
  if !checksum = Int.min_int then
    panic "unreachable parse benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
