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

let load_fixture = fun ~name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read lower benchmark fixture: " ^ Path.to_string path) in
  make_fixture ~name ~path source

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

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

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
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"parameterized let" ~path:(Path.v "sample.ml") "let id x = x\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"typed let heads" ~path:(Path.v "sample.ml") "let value : int = 1\nlet id x : int = x\nlet keep_pattern (x : int) = x\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"mutual recursive let" ~path:(Path.v "sample.ml") "let rec f = g\nand g = f\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"local let expression" ~path:(Path.v "sample.ml") "let x = let y = 1 in y\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"function expression" ~path:(Path.v "sample.ml") "let id = fun x -> x\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"match expression" ~path:(Path.v "sample.ml") "let value = match x with | 0 -> 1 | _ -> 2\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"list and array expressions" ~path:(Path.v "sample.ml") "let values = [1; 2]\nlet array = [|1; 2|]\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"labeled arguments" ~path:(Path.v "sample.ml") "let f ~x ?y = g ~x ?y\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"polymorphic variants" ~path:(Path.v "sample.ml") "let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"selectors and indexes" ~path:(Path.v "sample.ml") "let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"try expression" ~path:(Path.v "sample.ml") "let value = try read () with | Failure -> 0\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"loops" ~path:(Path.v "sample.ml") "let poll = while ready do step () done\nlet up = for i = 0 to n do step i done\nlet down = for i = n downto 0 do step i done\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"lazy exception interval patterns" ~path:(Path.v "sample.ml") "let force = function | lazy value -> value\nlet recovered = match read () with | exception Failure -> 0 | value -> value\nlet classify = function | 'a' .. 'z' -> 1 | _ -> 0\n");
  compare_fixture
    ~config:medium_config
    (load_fixture
      ~name:"atoms fixture"
      (Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml"));
  compare_fixture
    ~config:medium_config
    (load_fixture
      ~name:"nested fun fixture"
      (Path.v "packages/krasny/tests/fixtures/0415_nested_fun_parameter_stability.ml"));
  compare_fixture
    ~config:medium_config
    (load_fixture
      ~name:"multiline list fixture"
      (Path.v "packages/krasny/tests/fixtures/0952_multiline_list_expression_no_trailing_separator.ml"));
  compare_fixture
    ~config:medium_config
    (load_fixture
      ~name:"top-level let rec fixture"
      (Path.v "packages/krasny/tests/fixtures/0981_top_level_letrec_blank_line.ml"));
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
