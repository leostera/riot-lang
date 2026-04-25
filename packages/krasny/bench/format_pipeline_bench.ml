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

type counting_sink = {
  mutable bytes: int;
}

let counting_writer =
  let module Write = struct
    type t = counting_sink

    let write = fun sink ~from ->
      let bytes = IO.Buffer.readable_bytes from in
      sink.bytes <- sink.bytes + bytes;
      Ok bytes

    let write_vectored = fun sink ~from ->
      let bytes = ref 0 in
      IO.IoVec.for_each from ~fn:(fun slice -> bytes := !bytes + IO.IoSlice.length slice);
      sink.bytes <- sink.bytes + !bytes;
      Ok !bytes

    let flush = fun _sink -> Ok ()
  end in
  fun () ->
    let sink = { bytes = 0 } in
    (sink, IO.Writer.from_sink (module Write) sink)

let write_string_to_sink = fun formatted ->
  let sink, writer = counting_writer () in
  IO.write_all writer ~from:(IO.Buffer.from_string formatted) |> Result.expect ~msg:"format pipeline benchmark should write format1 output";
  IO.flush writer |> Result.expect ~msg:"format pipeline benchmark should flush format1 output";
  touch_int sink.bytes

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
  | Ok formatted -> formatted
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

let unicode_size_hint = String.length unicode_tables_fixture.source + 1

let memo = fun compute ->
  let value = ref None in
  fun () ->
    match !value with
    | Some value -> value
    | None ->
        let computed = compute () in
        value := Some computed;
        computed

let unicode_lower1_doc =
  memo (fun () -> parse1 () |> build_cst |> lower1)

let old_solve_print = fun doc ->
  doc
  |> Krasny.Solver.solve ~width:100
  |> Krasny.Printer.to_string ~size_hint:unicode_size_hint ~final_newline:true

let stream_solve_print = fun doc ->
  Krasny.Solver.to_string ~width:100 ~size_hint:unicode_size_hint ~final_newline:true doc

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
  let formatted = parse2 () |> ast2_source_file |> lower2 in
  touch_string formatted

let bench_solve1 = fun () ->
  let _doc = parse1 () |> build_cst |> lower1 |> solve in
  touch_int 1

let bench_print1 = fun () ->
  let formatted = parse1 () |> build_cst |> lower1 |> solve |> print1 in
  touch_string formatted

let bench_old_solve_print1 = fun () -> unicode_lower1_doc () |> old_solve_print |> touch_string

let bench_stream_solve_print1 = fun () -> unicode_lower1_doc () |> stream_solve_print |> touch_string

let bench_format1_to_sink = fun () ->
  let formatted = parse1 () |> build_cst |> lower1 |> solve |> print1 in
  write_string_to_sink formatted

let bench_write2_to_sink = fun () ->
  let sink, writer = counting_writer () in
  Krasny.stream_format (parse2 ()) ~writer ~width:100 |> Result.expect ~msg:"streaming format pipeline benchmark should write unicode tables";
  touch_int sink.bytes

let huge_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let solver_config: Bench.bench_config = { iterations = 5; warmup = 10 }

let make_case = fun name fn -> Bench.make_case_with_config ~config:huge_config name fn

let make_solver_case = fun name fn -> Bench.make_case_with_config ~config:solver_config name fn

let compare_stage = fun stage old_case new_case ->
  Bench.compare
    ("krasny pipeline: " ^ stage ^ " " ^ unicode_tables_fixture.name)
    [ make_case "old" old_case; make_case "new" new_case; ]

let compare_solver = fun doc_name old_case stream_case ->
  Bench.compare
    ("krasny solver: solve+print " ^ doc_name ^ " " ^ unicode_tables_fixture.name)
    [
      make_solver_case "old solve+print" old_case;
      make_solver_case "stream solve+print" stream_case;
    ]

let benchmarks = [
  compare_stage "parse" bench_parse1 bench_parse2;
  compare_stage "source repr" bench_source1 bench_source2;
  compare_stage "format from tree" bench_print1 bench_lower2;
  compare_solver "lower1 doc" bench_old_solve_print1 bench_stream_solve_print1;
  Bench.compare
    ("krasny pipeline: format to writer " ^ unicode_tables_fixture.name)
    [
      make_case "format1 string->writer" bench_format1_to_sink;
      make_case "Krasny.stream_format" bench_write2_to_sink;
    ];
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
