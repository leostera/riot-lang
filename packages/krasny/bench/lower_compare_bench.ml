open Std

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let make_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create lower benchmark source slice"

let make_fixture = fun ~name ~path source -> { name; path; source; slice = make_slice source }

let touch_formatted = fun formatted -> checksum := !checksum lxor String.length formatted

let bench_lower1 = fun fixture ->
  let parsed = Syn.parse ~filename:fixture.path fixture.source in
  let formatted = Krasny.format parsed |> Result.expect ~msg:"lower1 benchmark source should format" in
  touch_formatted formatted

let bench_lower2 = fun fixture ->
  let parsed = Syn.parse2 ~filename:fixture.path fixture.slice in
  let formatted = Krasny.format2 parsed |> Result.expect ~msg:"lower2 benchmark source should format" in
  touch_formatted formatted

let tiny_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 500; warmup = 50 }

let make_lower_case = fun ~config name fn -> Bench.make_case_with_config ~config name fn

let compare_fixture = fun ~config fixture ->
  Bench.compare
    ("krasny lower: " ^ fixture.name)
    [
      make_lower_case ~config "parse1 + lower1" (fun () -> bench_lower1 fixture);
      make_lower_case ~config "parse2 + lower2" (fun () -> bench_lower2 fixture);
    ]

let benchmarks = [
  compare_fixture
    ~config:tiny_config
    (make_fixture ~name:"tiny let binding" ~path:(Path.v "sample.ml") "let x = 1 + 2\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"open declaration" ~path:(Path.v "sample.ml") "open Foo.Bar\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"type alias" ~path:(Path.v "sample.mli") "type 'a t = 'a list\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"value declaration" ~path:(Path.v "sample.mli") "val id : 'a -> 'a\n");
]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"krasny lower comparison" ~benchmarks ~args in
      if !checksum = Int.min_int then
        panic "unreachable lower benchmark checksum";
      result)
    ~args:Env.args
    ()
