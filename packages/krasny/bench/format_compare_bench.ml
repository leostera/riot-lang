open Std
open Std.Collections

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let make_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create format benchmark source slice"

let load_fixture = fun ~name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read format benchmark fixture: " ^ Path.to_string path) in
  { name; path; source; slice = make_slice source }

let touch_formatted = fun formatted -> checksum := !checksum lxor String.length formatted

let finalize_rendered_output = fun rendered ->
  if String.length rendered = 0 || String.ends_with ~suffix:"\n" rendered then
    rendered
  else
    rendered ^ "\n"

let build_cst_error_to_string = function
  | Syn.Parse_diagnostics diagnostics -> "parse diagnostics: "
  ^ Int.to_string (List.length diagnostics)
  | Syn.Cst_builder_error error -> "CST builder error: " ^ error.Syn.CstBuilder.message

let bench_format1 = fun fixture ->
  let parsed = Syn.parse ~filename:fixture.path fixture.source in
  match Syn.build_cst parsed with
  | Error error -> panic
    ("format1 failed to build CST for "
    ^ Path.to_string fixture.path
    ^ ": "
    ^ build_cst_error_to_string error)
  | Ok source_file -> (
      match Krasny.Lower.source_file source_file with
      | Error error -> panic
        ("format1 failed to lower "
        ^ Path.to_string fixture.path
        ^ ": "
        ^ Krasny.Lower.error_to_string error)
      | Ok rendered ->
          let formatted = rendered |> Krasny.Solver.solve ~width:100 |> Krasny.Printer.to_string |> finalize_rendered_output in
          touch_formatted formatted
    )

let bench_format2 = fun fixture ->
  let parsed = Syn.parse2 ~filename:fixture.path fixture.slice in
  let formatted = Krasny.format2 parsed
  |> Result.expect ~msg:("format2 should format " ^ Path.to_string fixture.path) in
  touch_formatted formatted

let tiny_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 250; warmup = 25 }

let medium_config: Bench.bench_config = { iterations = 25; warmup = 5 }

let large_config: Bench.bench_config = { iterations = 5; warmup = 1 }

let huge_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let make_case = fun ~config name fn -> Bench.make_case_with_config ~config name fn

let compare_fixture = fun ~config fixture ->
  Bench.compare
    ("krasny format: " ^ fixture.name)
    [
      make_case ~config "format1" (fun () -> bench_format1 fixture);
      make_case ~config "format2" (fun () -> bench_format2 fixture);
    ]

let benchmarks = [
  compare_fixture
    ~config:tiny_config
    {
      name = "tiny let binding";
      path = Path.v "sample.ml";
      source = "let x = 1 + 2\n";
      slice = make_slice "let x = 1 + 2\n"
    };
  compare_fixture
    ~config:small_config
    (load_fixture
      ~name:"atoms fixture"
      (Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml"));
  compare_fixture
    ~config:medium_config
    (load_fixture ~name:"config implementation" (Path.v "packages/std/src/config/config.ml"));
  compare_fixture
    ~config:large_config
    (load_fixture ~name:"parser2 implementation" (Path.v "packages/syn/src/parser2.ml"));
  compare_fixture
    ~config:huge_config
    (load_fixture
      ~name:"unicode tables implementation"
      (Path.v "packages/std/src/unicode/unicode_tables.ml"));
]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"krasny format comparison" ~benchmarks ~args in
      if !checksum = Int.min_int then
        panic "unreachable format benchmark checksum";
      result)
    ~args:Env.args
    ()
