open Std
open Std.Collections

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create format benchmark source slice"

let load_fixture = fun ~name path ->
  let source =
    Fs.read path
    |> Result.expect ~msg:("failed to read format benchmark fixture: " ^ Path.to_string path)
  in
  {
    name;
    path;
    source;
    slice = source_slice source;
  }

let touch_int = fun value -> checksum := !checksum lxor value

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

let bench_format_to_string = fun fixture ->
  let formatted =
    parse fixture
    |> Krasny.format
    |> Result.expect ~msg:"format benchmark should format to string"
  in
  touch_int (String.length formatted)

let bench_stream_format = fun fixture ->
  let (sink, writer) = counting_writer () in
  Krasny.stream_format (parse fixture) ~writer ~width:100
  |> Result.expect ~msg:"format benchmark should stream to writer";
  touch_int sink.bytes

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
      make_case ~config "Krasny.format" (fun () -> bench_format_to_string fixture);
      make_case ~config "Krasny.stream_format" (fun () -> bench_stream_format fixture);
    ]

let benchmarks = [
  compare_fixture
    ~config:tiny_config
    {
      name = "tiny let binding";
      path = Path.v "sample.ml";
      source = "let x = 1 + 2\n";
      slice = source_slice "let x = 1 + 2\n";
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
    (load_fixture ~name:"parser implementation" (Path.v "packages/syn/src/parser.ml"));
  compare_fixture
    ~config:huge_config
    (load_fixture
      ~name:"unicode tables implementation"
      (Path.v "packages/std/src/unicode/unicode_tables.ml"));
]

let main ~args =
  let result = Bench.Cli.main ~name:"krasny format comparison" ~benchmarks ~args in
  if !checksum = Int.min_int then
    panic "unreachable format benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
