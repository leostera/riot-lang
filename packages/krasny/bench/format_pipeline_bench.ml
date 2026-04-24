open Std
open Std.Collections

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

type 'a memo = {
  mutable value: 'a option;
}

let checksum = ref 0

let make_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create format pipeline benchmark source slice"

let load_fixture = fun ~name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read format pipeline benchmark fixture: " ^ Path.to_string path) in
  { name; path; source; slice = make_slice source }

let unicode_tables_fixture = load_fixture
  ~name:"unicode tables implementation"
  (Path.v "packages/std/src/unicode/unicode_tables.ml")

let memo = fun () -> { value = None }

let memoized = fun cache compute ->
  match cache.value with
  | Some value -> value
  | None ->
      let value = compute () in
      cache.value <- Some value;
      value

let cst_source_file_cache = memo ()

let ast2_source_file_cache = memo ()

let lower1_doc_cache = memo ()

let lower2_doc_cache = memo ()

let solved1_doc_cache = memo ()

let solved2_doc_cache = memo ()

let cst_source_file = fun () ->
  memoized cst_source_file_cache
    (fun () ->
      match Syn.build_cst
        (Syn.parse ~filename:unicode_tables_fixture.path unicode_tables_fixture.source) with
      | Ok source_file -> source_file
      | Error error ->
          let message =
            match error with
            | Syn.Parse_diagnostics diagnostics -> "parse diagnostics: "
            ^ Int.to_string (List.length diagnostics)
            | Syn.Cst_builder_error err -> "CST builder error: " ^ err.Syn.CstBuilder.message
          in
          panic
            ("failed to build CST for " ^ Path.to_string unicode_tables_fixture.path ^ ": " ^ message))

let ast2_source_file = fun () ->
  memoized
    ast2_source_file_cache
    (fun () ->
      Syn.Ast2.SourceFile.make
        (Syn.parse2 ~filename:unicode_tables_fixture.path unicode_tables_fixture.slice).Syn.Parser2.tree)

let lower1_doc = fun () ->
  memoized lower1_doc_cache
    (fun () ->
      match Krasny.Lower.source_file (cst_source_file ()) with
      | Ok doc -> doc
      | Error error -> panic
        ("format pipeline lower1 failed for "
        ^ Path.to_string unicode_tables_fixture.path
        ^ ": "
        ^ Krasny.Lower.error_to_string error))

let lower2_doc = fun () ->
  memoized lower2_doc_cache
    (fun () ->
      match Krasny.Lower2.source_file (ast2_source_file ()) with
      | Ok doc -> doc
      | Error error -> panic
        ("format pipeline lower2 failed for "
        ^ Path.to_string unicode_tables_fixture.path
        ^ ": "
        ^ Krasny.Lower2.error_to_string error))

let solved1_doc = fun () ->
  memoized solved1_doc_cache (fun () -> Krasny.Solver.solve ~width:100 (lower1_doc ()))

let solved2_doc = fun () ->
  memoized solved2_doc_cache (fun () -> Krasny.Solver.solve ~width:100 (lower2_doc ()))

let touch_int = fun value -> checksum := !checksum lxor value

let touch_string = fun value -> checksum := !checksum lxor String.length value

let bench_parse1 = fun () ->
  let result = Syn.parse ~filename:unicode_tables_fixture.path unicode_tables_fixture.source in
  let width = Syn.Ceibo.Green.width (Syn.Ceibo.Green.Node result.Syn.Parser.tree) in
  touch_int width

let bench_parse2 = fun () ->
  let result = Syn.parse2 ~filename:unicode_tables_fixture.path unicode_tables_fixture.slice in
  let width = (Syn.SyntaxTree.root result.Syn.Parser2.tree).Syn.SyntaxTree.full_width in
  touch_int width

let bench_lower1 = fun () ->
  touch_int
    (
      if Krasny.Doc.is_multiline (lower1_doc ()) then
        1
      else
        0
    )

let bench_lower2 = fun () ->
  touch_int
    (
      if Krasny.Doc.is_multiline (lower2_doc ()) then
        1
      else
        0
    )

let bench_solve1 = fun () ->
  let doc = Krasny.Solver.solve ~width:100 (lower1_doc ()) in
  touch_int
    (
      if Krasny.Doc.is_multiline doc then
        1
      else
        0
    )

let bench_solve2 = fun () ->
  let doc = Krasny.Solver.solve ~width:100 (lower2_doc ()) in
  touch_int
    (
      if Krasny.Doc.is_multiline doc then
        1
      else
        0
    )

let bench_print1 = fun () -> solved1_doc () |> Krasny.Printer.to_string |> touch_string

let bench_print2 = fun () -> solved2_doc () |> Krasny.Printer.to_string |> touch_string

let bench_format1 = fun () -> solved1_doc () |> Krasny.Printer.to_string |> touch_string

let bench_format2 = fun () ->
  Krasny.format2 (Syn.parse2 ~filename:unicode_tables_fixture.path unicode_tables_fixture.slice)
  |> Result.expect ~msg:"format2 pipeline benchmark should format unicode tables"
  |> touch_string

let huge_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let make_case = fun name fn -> Bench.make_case_with_config ~config:huge_config name fn

let benchmarks = [ Bench.compare "krasny pipeline: unicode tables implementation"
    [
      make_case "parse1" bench_parse1;
      make_case "parse2" bench_parse2;
      make_case "lower1" bench_lower1;
      make_case "lower2" bench_lower2;
      make_case "solve1" bench_solve1;
      make_case "solve2" bench_solve2;
      make_case "print1" bench_print1;
      make_case "print2" bench_print2;
      make_case "format1" bench_format1;
      make_case "format2" bench_format2;
    ]; ]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"krasny format pipeline" ~benchmarks ~args in
      if !checksum = Int.min_int then
        panic "unreachable format pipeline benchmark checksum";
      result)
    ~args:Env.args
    ()
