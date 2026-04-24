open Std
open Std.Collections

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
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

let touch_int = fun value -> checksum := !checksum lxor value

let touch_string = fun value -> checksum := !checksum lxor String.length value

let finalize_rendered_output = fun rendered ->
  if String.length rendered = 0 || String.ends_with ~suffix:"\n" rendered then
    rendered
  else
    rendered ^ "\n"

let build_cst_error_to_string = function
  | Syn.Parse_diagnostics diagnostics -> "parse diagnostics: "
  ^ Int.to_string (List.length diagnostics)
  | Syn.Cst_builder_error err -> "CST builder error: " ^ err.Syn.CstBuilder.message

let parse2_diagnostics_to_string = fun diagnostics ->
  let count = Vector.length diagnostics in
  if count = 0 then
    "parse2 diagnostics prevented formatting"
  else
    let first = Vector.get_unchecked diagnostics ~at:0 |> Syn.Diagnostic.to_string in
    if count = 1 then
      first
    else
      first ^ " (+" ^ Int.to_string (count - 1) ^ " more)"

let parse1 = fun () -> Syn.parse ~filename:unicode_tables_fixture.path unicode_tables_fixture.source

let parse2 = fun () -> Syn.parse2 ~filename:unicode_tables_fixture.path unicode_tables_fixture.slice

let build_cst = fun parsed ->
  match Syn.build_cst parsed with
  | Ok source_file -> source_file
  | Error error -> panic
    ("format pipeline benchmark failed to build CST for "
    ^ Path.to_string unicode_tables_fixture.path
    ^ ": "
    ^ build_cst_error_to_string error)

let ast2_source_file = fun (parsed: Syn.Parser2.parse_result) ->
  let diagnostics = parsed.Syn.Parser2.diagnostics in
  if Vector.length diagnostics > 0 then
    panic
      ("format pipeline benchmark failed to parse2 "
      ^ Path.to_string unicode_tables_fixture.path
      ^ ": "
      ^ parse2_diagnostics_to_string diagnostics)
  else
    Syn.Ast2.SourceFile.make parsed.Syn.Parser2.tree

let lower1 = fun source_file ->
  match Krasny.Lower.source_file source_file with
  | Ok doc -> doc
  | Error error -> panic
    ("format pipeline benchmark failed to lower1 "
    ^ Path.to_string unicode_tables_fixture.path
    ^ ": "
    ^ Krasny.Lower.error_to_string error)

let lower2 = fun source_file ->
  match Krasny.Lower2.source_file source_file with
  | Ok doc -> doc
  | Error error -> panic
    ("format pipeline benchmark failed to lower2 "
    ^ Path.to_string unicode_tables_fixture.path
    ^ ": "
    ^ Krasny.Lower2.error_to_string error)

let solve = fun doc -> Krasny.Solver.solve ~width:100 doc

let print1 = fun doc -> doc |> Krasny.Printer.to_string |> finalize_rendered_output

let print2 = fun doc ->
  Krasny.Printer.to_string
    ~size_hint:(String.length unicode_tables_fixture.source + 1)
    ~final_newline:true
    doc

let bench_parse1 = fun () ->
  let result = parse1 () in
  let width = Syn.Ceibo.Green.width (Syn.Ceibo.Green.Node result.Syn.Parser.tree) in
  touch_int width

let bench_parse2 = fun () ->
  let result = parse2 () in
  let width = (Syn.SyntaxTree.root result.Syn.Parser2.tree).Syn.SyntaxTree.full_width in
  touch_int width

let bench_source1 = fun () ->
  let source_file = parse1 () |> build_cst in
  match Syn.Cst.SourceFile.kind source_file with
  | `Implementation -> touch_int 1
  | `Interface -> touch_int 2

let bench_source2 = fun () ->
  let source_file = parse2 () |> ast2_source_file in
  touch_int (Syn.Ast2.Node.full_width source_file)

let bench_lower1 = fun () ->
  let _doc = parse1 () |> build_cst |> lower1 in
  touch_int 1

let bench_lower2 = fun () ->
  let _doc = parse2 () |> ast2_source_file |> lower2 in
  touch_int 1

let bench_solve1 = fun () ->
  let _doc = parse1 () |> build_cst |> lower1 |> solve in
  touch_int 1

let bench_solve2 = fun () ->
  let _doc = parse2 () |> ast2_source_file |> lower2 |> solve in
  touch_int 1

let bench_print1 = fun () ->
  let formatted = parse1 () |> build_cst |> lower1 |> solve |> print1 in
  touch_string formatted

let bench_print2 = fun () ->
  let formatted = parse2 () |> ast2_source_file |> lower2 |> solve |> print2 in
  touch_string formatted

let bench_format1 = bench_print1

let bench_format2 = fun () ->
  Krasny.format2 (parse2 ())
  |> Result.expect ~msg:"format2 pipeline benchmark should format unicode tables"
  |> touch_string

let huge_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let make_case = fun name fn -> Bench.make_case_with_config ~config:huge_config name fn

let compare_stage = fun stage old_case new_case ->
  Bench.compare
    ("krasny pipeline: " ^ stage ^ " " ^ unicode_tables_fixture.name)
    [ make_case "old" old_case; make_case "new" new_case; ]

let benchmarks = [
  compare_stage "parse" bench_parse1 bench_parse2;
  compare_stage "source repr" bench_source1 bench_source2;
  compare_stage "lower cumulative" bench_lower1 bench_lower2;
  compare_stage "solve cumulative" bench_solve1 bench_solve2;
  compare_stage "print cumulative" bench_print1 bench_print2;
  compare_stage "format path" bench_format1 bench_format2;
]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"krasny format pipeline" ~benchmarks ~args in
      if !checksum = Int.min_int then
        panic "unreachable format pipeline benchmark checksum";
      result)
    ~args:Env.args
    ()
