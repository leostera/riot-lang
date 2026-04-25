open Std
open Std.Collections

type fixture = { name: string; path: Path.t; source: string; slice: IO.IoVec.IoSlice.t }

let checksum = ref 0

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create format pipeline benchmark source slice"

let load_fixture = fun ~name path ->
  let source = Fs.read path |> Result.expect ~msg:("failed to read format pipeline benchmark fixture: " ^ Path.to_string path) in
  {
    name;
    path;
    source;
    slice = source_slice source
  }

let unicode_tables_fixture = load_fixture ~name:"unicode tables implementation" (Path.v "packages/std/src/unicode/unicode_tables.ml")

let touch_int = fun value -> checksum := !checksum lxor value

let touch_string = fun value -> checksum := !checksum lxor String.length value

type counting_sink = { mutable bytes: int }

let counting_writer =
  let module Write = struct
    type t = counting_sink

    let write = fun sink ~from ->
      let bytes = IO.Buffer.readable_bytes from in
      sink.bytes <- sink.bytes + bytes;
      Ok bytes

    let write_vectored = fun sink ~from ->
      let bytes = ref 0 in
      IO.IoVec.for_each from ~fn:(
        fun slice -> bytes := !bytes + IO.IoSlice.length slice
      );
      sink.bytes <- sink.bytes + !bytes;
      Ok !bytes

    let flush = fun _sink -> Ok ()
  end in
  fun () ->
    let sink = { bytes = 0 } in (sink, IO.Writer.from_sink (module Write) sink)

let parse = fun () -> Syn.parse ~filename:unicode_tables_fixture.path unicode_tables_fixture.slice

let bench_parse = fun () ->
  let result = parse () in
  let root = Syn.SyntaxTree.root result.Syn.Parser.tree in touch_int root.Syn.SyntaxTree.full_width

let bench_ast_view = fun () ->
  let result = parse () in
  let source_file = Syn.Ast.SourceFile.make result.Syn.Parser.tree in touch_int (Syn.Ast.Node.full_width source_file)

let bench_format_to_string = fun () ->
  let formatted = parse () |> Krasny.format |> Result.expect ~msg:"format pipeline benchmark should format to string" in touch_string formatted

let bench_stream_format = fun () ->
  let sink, writer = counting_writer () in
  Krasny.stream_format (parse ()) ~writer ~width:100 |> Result.expect ~msg:"format pipeline benchmark should stream unicode tables";
  touch_int sink.bytes

let huge_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let make_case = fun name fn -> Bench.make_case_with_config ~config:huge_config name fn

let benchmarks = [ Bench.compare ("krasny pipeline: parse " ^ unicode_tables_fixture.name) [ make_case "parse" bench_parse; make_case "parse + Ast view" bench_ast_view ]; Bench.compare ("krasny pipeline: format to output " ^ unicode_tables_fixture.name) [ make_case "Krasny.format" bench_format_to_string; make_case "Krasny.stream_format" bench_stream_format ] ]

let main ~args =
  let result = Bench.Cli.main ~name:"krasny format pipeline" ~benchmarks ~args in
  if !checksum = Int.min_int then
    panic "unreachable format pipeline benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
