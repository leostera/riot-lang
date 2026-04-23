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

let make_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create parse2 benchmark source slice"

let load_fixture = fun name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read parse2 benchmark fixture: " ^ Path.to_string path) in
  { name; path; source; slice = make_slice source }

let touch_parse2 = fun (result: Parser2.parse_result) ->
  let root = SyntaxTree.root result.Parser2.tree in
  checksum := !checksum lxor root.SyntaxTree.full_width lxor Vector.length result.Parser2.diagnostics

let bench_parse2 = fun fixture ->
  let result = parse2 ~filename:fixture.path fixture.slice in
  touch_parse2 result

let is_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let has_lossless_snapshot = fun path ->
  let snapshot_path = Path.to_string path ^ ".expected_lossless.json"
  |> Path.from_string
  |> Result.expect ~msg:"lossless snapshot path should stay valid UTF-8" in
  Fs.exists snapshot_path |> Result.unwrap_or ~default:false

let load_valid_fixture_corpus = fun () ->
  let fixtures = Vector.with_capacity ~size:1_100 in
  Fs.Walker.walk ~roots:[ Path.v "packages/syn/tests/fixtures" ] ~sort:true
    ~f:(fun item ->
      let path = Fs.Walker.FileItem.path item in
      (
        if is_source_file path && has_lossless_snapshot path then
          let name = Fs.Walker.FileItem.name item in
          Vector.push fixtures ~value:(load_fixture name path)
      );
      Fs.Walker.Continue)
    () |> Result.expect ~msg:"failed to walk syn fixture corpus";
  fixtures

let bench_parse2_corpus = fun fixtures ->
  let rec loop index =
    if index < Vector.length fixtures then
      (
        bench_parse2 (Vector.get_unchecked fixtures ~at:index);
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
  Bench.with_config ~config ("parse2: " ^ fixture.name) (fun () -> bench_parse2 fixture)

let selected_benchmarks = fun () ->
  [
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
      (load_fixture "docstrings" (Path.v "packages/syn/tests/fixtures/ocaml_docstrings.ml"));
    fixture_benchmark
      ~config:large_config
      (load_fixture "large cst fixture" (Path.v "packages/syn/tests/deps_fixtures/0029_cst_tests.ml"));
  ]

let corpus_benchmark = fun () ->
  let fixtures = load_valid_fixture_corpus () in
  Bench.with_config
    ~config:corpus_config
    ("parse2: valid fixture corpus (" ^ Int.to_string (Vector.length fixtures) ^ " files)")
    (fun () -> bench_parse2_corpus fixtures)

let benchmarks = fun () -> selected_benchmarks () @ [ corpus_benchmark () ]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"syn parse2" ~benchmarks:(benchmarks ()) ~args in
      if !checksum = Int.min_int then
        panic "unreachable parse2 benchmark checksum";
      result)
    ~args:Env.args
    ()
