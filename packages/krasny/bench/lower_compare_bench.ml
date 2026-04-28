open Std
open Std.Collections

type source_fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let touch_int = fun value -> checksum := !checksum lxor value

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create lower benchmark source slice"

let make_fixture = fun ~name ~path source ->
  {
    name;
    path;
    source;
    slice = source_slice source;
  }

let load_fixture = fun ~name path ->
  let source =
    Fs.read path
    |> Result.expect ~msg:("failed to read lower benchmark fixture: " ^ Path.to_string path)
  in
  make_fixture ~name ~path source

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
      IO.IoVec.for_each from ~fn:(fun slice -> bytes := !bytes + IO.IoSlice.length slice);
      sink.bytes <- sink.bytes + !bytes;
      Ok !bytes

    let flush = fun _sink -> Ok ()
  end in
  fun () ->
    let sink = { bytes = 0 } in
    (sink, IO.Writer.from_sink (module Write) sink)

let parse = fun fixture -> Syn.parse ~filename:fixture.path fixture.slice

let bench_ast_view = fun fixture ->
  let parsed = parse fixture in
  if Vector.length parsed.Syn.Parser.diagnostics > 0 then
    panic ("lower benchmark parse failed for " ^ Path.to_string fixture.path);
  let source_file = Syn.Ast.SourceFile.make parsed.Syn.Parser.tree in
  touch_int (Syn.Ast.SourceFile.full_width source_file)

let bench_stream_format = fun fixture ->
  let parsed = parse fixture in
  let (sink, writer) = counting_writer () in
  Krasny.stream_format parsed ~writer ~width:100
  |> Result.expect ~msg:("stream format benchmark failed for " ^ Path.to_string fixture.path);
  touch_int sink.bytes

let tiny_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 500; warmup = 50 }

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

let benchmark_fixture = fun ~config fixture ->
  Bench.compare
    ("krasny lower: " ^ fixture.name)
    [
      Bench.make_case_with_config ~config "ast view" (fun () -> bench_ast_view fixture);
      Bench.make_case_with_config ~config "stream format" (fun () -> bench_stream_format fixture);
    ]

let source_fixtures = [
  (tiny_config, make_fixture ~name:"tiny let binding" ~path:(Path.v "sample.ml") "let x = 1 + 2\n");
  (small_config, make_fixture ~name:"open declaration" ~path:(Path.v "sample.ml") "open Foo.Bar\n");
  (small_config, make_fixture ~name:"parameterized let" ~path:(Path.v "sample.ml") "let id x = x\n");
  (
    small_config,
    make_fixture
      ~name:"match expression"
      ~path:(Path.v "sample.ml")
      "let value = match x with | 0 -> 1 | _ -> 2\n"
  );
  (
    small_config,
    make_fixture
      ~name:"module and module type declarations"
      ~path:(Path.v "sample.ml")
      "module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n"
  );
  (
    small_config,
    make_fixture ~name:"value declaration" ~path:(Path.v "sample.mli") "val id : 'a -> 'a\n"
  );
  (
    medium_config,
    load_fixture
      ~name:"atoms fixture"
      (Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml")
  );
  (
    medium_config,
    load_fixture
      ~name:"bindings and control flow fixture"
      (Path.v "packages/krasny/tests/fixtures/0300_bindings_and_control_flow.ml")
  );
]

let benchmarks = fun () ->
  List.map
    source_fixtures
    ~fn:(fun (config, fixture) -> benchmark_fixture ~config fixture)

let main ~args =
  let result = Bench.Cli.main ~name:"krasny stream format" ~benchmarks:(benchmarks ()) ~args in
  if !checksum = Int.min_int then
    panic "unreachable lower benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
