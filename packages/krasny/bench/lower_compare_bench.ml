open Std
open Std.Collections

type source_fixture = {
  name: string;
  path: Path.t;
  source: string;
}

type fixture = {
  fixture_name: string;
  fixture_path: Path.t;
  old_source_file: Syn.Cst.SourceFile.t;
  new_source_file: Syn.Ast2.SourceFile.t;
}

let checksum = ref 0

let touch_int = fun value -> checksum := !checksum lxor value

let build_cst_error_to_string = function
  | Syn.Parse_diagnostics diagnostics -> "parse diagnostics: "
  ^ Int.to_string (List.length diagnostics)
  | Syn.Cst_builder_error err -> "CST builder error: " ^ err.Syn.CstBuilder.message

let parse2_diagnostics_to_string = fun diagnostics ->
  let count = Vector.length diagnostics in
  if count = 0 then
    "parse2 diagnostics prevented lowering"
  else
    let first = Vector.get_unchecked diagnostics ~at:0 |> Syn.Diagnostic.to_string in
    if count = 1 then
      first
    else
      first ^ " (+" ^ Int.to_string (count - 1) ^ " more)"

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create lower benchmark source slice"

let parse1_source_file = fun ~path source ->
  let parsed = Syn.parse ~filename:path source in
  match Syn.build_cst parsed with
  | Ok source_file -> source_file
  | Error error -> panic
    ("lower benchmark failed to build CST for "
    ^ Path.to_string path
    ^ ": "
    ^ build_cst_error_to_string error)

let parse2_source_file = fun ~path source ->
  let parsed = Syn.parse2 ~filename:path (source_slice source) in
  let diagnostics = parsed.Syn.Parser2.diagnostics in
  if Vector.length diagnostics > 0 then
    panic
      ("lower benchmark failed to parse2 "
      ^ Path.to_string path
      ^ ": "
      ^ parse2_diagnostics_to_string diagnostics)
  else
    Syn.Ast2.SourceFile.make parsed.Syn.Parser2.tree

let make_fixture = fun ~name ~path source -> { name; path; source }

let load_fixture = fun ~name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read lower benchmark fixture: " ^ Path.to_string path) in
  make_fixture ~name ~path source

let prepare_fixture = fun fixture ->
  {
    fixture_name = fixture.name;
    fixture_path = fixture.path;
    old_source_file = parse1_source_file ~path:fixture.path fixture.source;
    new_source_file = parse2_source_file ~path:fixture.path fixture.source
  }

let touch_old_doc = function
  | Krasny.Doc.Empty -> touch_int 0
  | Krasny.Doc.Text text
  | Krasny.Doc.RawText text -> touch_int (String.length text)
  | Krasny.Doc.Slice slice -> touch_int (IO.IoVec.IoSlice.length slice.value)
  | Krasny.Doc.Space -> touch_int 1
  | Krasny.Doc.Spaces count -> touch_int count
  | Krasny.Doc.Line -> touch_int 2
  | Krasny.Doc.Break flat -> touch_int (String.length flat)
  | Krasny.Doc.Group _ -> touch_int 3
  | Krasny.Doc.Concat docs -> touch_int (Vector.length docs)
  | Krasny.Doc.Indent (spaces, _) -> touch_int spaces

let lower1 = fun fixture ->
  match Krasny.Lower.source_file fixture.old_source_file with
  | Ok doc -> touch_old_doc doc
  | Error error -> panic
    ("lower benchmark failed to lower1 "
    ^ Path.to_string fixture.fixture_path
    ^ ": "
    ^ Krasny.Lower.error_to_string error)

let format2_from_ast = fun fixture ->
  match Krasny.Lower2.source_file fixture.new_source_file with
  | Ok formatted -> touch_int (String.length formatted)
  | Error error -> panic
    ("lower benchmark failed to format2 from ast "
    ^ Path.to_string fixture.fixture_path
    ^ ": "
    ^ Krasny.Lower2.error_to_string error)

let tiny_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 500; warmup = 50 }

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

let compare_fixture = fun ~config fixture -> (config, fixture)

let benchmark_fixture = fun ~config fixture ->
  Bench.compare
    ("krasny lower: " ^ fixture.fixture_name)
    [
      Bench.make_case_with_config ~config "old" (fun () -> lower1 fixture);
      Bench.make_case_with_config ~config "new" (fun () -> format2_from_ast fixture);
    ]

let source_fixtures = [
  compare_fixture
    ~config:tiny_config
    (make_fixture ~name:"tiny let binding" ~path:(Path.v "sample.ml") "let x = 1 + 2\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"open declaration" ~path:(Path.v "sample.ml") "open Foo.Bar\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"include external exception declarations" ~path:(Path.v "sample.ml") "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"module and module type declarations" ~path:(Path.v "sample.ml") "module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"type alias" ~path:(Path.v "sample.mli") "type 'a t = 'a list\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"tuple type separators" ~path:(Path.v "sample.mli") "type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n");
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
    (make_fixture ~name:"function application argument" ~path:(Path.v "sample.ml") "let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"labeled arguments" ~path:(Path.v "sample.ml") "let f ~x ?y = g ~x ?y\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"polymorphic variants" ~path:(Path.v "sample.ml") "let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"attributes" ~path:(Path.v "sample.ml") "let value = target [@inline always]\nlet (x [@foo]) = value\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"extensions" ~path:(Path.v "sample.ml") "let value = [%expr payload]\nlet [%pat payload] = value\n[%%item payload]\n[@@@warning \"-32\"]\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"selectors and indexes" ~path:(Path.v "sample.ml") "let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"record expressions and patterns" ~path:(Path.v "sample.ml") "let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet { x; y = z; _ } = record\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"binding operator expressions" ~path:(Path.v "sample.ml") "let value = let* x = fetch in let+ y = decode in pair x y\nlet both = let+ x = a and+ y = b in pair x y\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"local open expressions and patterns" ~path:(Path.v "sample.ml") "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"first-class module expressions" ~path:(Path.v "sample.ml") "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"special patterns" ~path:(Path.v "sample.ml") "let f (type a b) (module M : S.T) = value\nlet g (module _) = value\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"let module expressions" ~path:(Path.v "sample.ml") "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"let exception expressions" ~path:(Path.v "sample.ml") "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n");
  compare_fixture
    ~config:small_config
    (make_fixture ~name:"unreachable expressions" ~path:(Path.v "sample.ml") "let value = match maybe with | Some value -> value | None -> .\n");
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
      ~name:"bindings and control flow fixture"
      (Path.v "packages/krasny/tests/fixtures/0300_bindings_and_control_flow.ml"));
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

let benchmarks = fun () ->
  List.map
    source_fixtures
    ~fn:(fun (config, source_fixture) -> source_fixture |> prepare_fixture |> benchmark_fixture ~config)

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"krasny lower comparison" ~benchmarks:(benchmarks ()) ~args in
      if !checksum = Int.min_int then
        panic "unreachable lower benchmark checksum";
      result)
    ~args:Env.args
    ()
