open Std
open Std.Collections

let sample_ml = Path.v "sample.ml"

let sample_mli = Path.v "sample.mli"

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ Kernel.IO.Error.message error)

let parse2_ml = fun source -> Syn.parse2 ~filename:sample_ml (source_slice source)

let parse2_mli = fun source -> Syn.parse2 ~filename:sample_mli (source_slice source)

let format2_ml = fun source -> parse2_ml source |> Krasny.format2

let format2_mli = fun source -> parse2_mli source |> Krasny.format2

let parse2_source = fun ~filename source -> Syn.parse2 ~filename (source_slice source)

let format2_source = fun ~filename source -> parse2_source ~filename source |> Krasny.format2

let assert_format2_ml = fun ~expected source ->
  let actual = format2_ml source |> Result.expect ~msg:"implementation should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 implementation output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_mli = fun ~expected source ->
  let actual = format2_mli source |> Result.expect ~msg:"interface should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 interface output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_ml_fails = fun source ->
  match format2_ml source with
  | Ok formatted -> Error ("lower2 unexpectedly formatted unsupported source as:\n" ^ formatted)
  | Error _ -> Ok ()

let assert_lower2_fixture_idempotent = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  match format2_source ~filename:path source with
  | Error err -> Error (Path.to_string path
  ^ " failed lower2 formatting: "
  ^ Krasny.format_error_to_string err)
  | Ok formatted -> (
      match format2_source ~filename:path formatted with
      | Error err -> Error (Path.to_string path
      ^ " formatted once but failed to format again: "
      ^ Krasny.format_error_to_string err)
      | Ok reformatted ->
          if formatted = reformatted then
            Ok ()
          else
            Error (Path.to_string path
            ^ " is not lower2-idempotent after one format\nfirst:\n"
            ^ formatted
            ^ "\nsecond:\n"
            ^ reformatted)
    )

let assert_lower2_existing_fixture_subset = fun () ->
  let fixtures = [
    Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml";
    Path.v "packages/krasny/tests/fixtures/0300_bindings_and_control_flow.ml";
    Path.v "packages/krasny/tests/fixtures/0415_nested_fun_parameter_stability.ml";
    Path.v "packages/krasny/tests/fixtures/0952_multiline_list_expression_no_trailing_separator.ml";
    Path.v "packages/krasny/tests/fixtures/0981_top_level_letrec_blank_line.ml";
  ] in
  let rec loop = function
    | [] -> Ok ()
    | path :: rest -> (
        match assert_lower2_fixture_idempotent path with
        | Ok () -> loop rest
        | Error _ as err -> err
      )
  in
  loop fixtures

let tests = [
  Test.case
    "lower2 keeps empty implementations empty"
    (fun _ctx -> assert_format2_ml ~expected:"" "");
  Test.case
    "lower2 formats simple let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1 + 2\n" "let x = 1 + 2\n");
  Test.case
    "lower2 formats parameterized let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let id x = x\n" "let id x = x\n");
  Test.case
    "lower2 formats typed let binding heads"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value: int = 1\nlet id x: int = x\nlet keep_pattern (x: int) = x\n"
        "let value : int = 1\nlet id x : int = x\nlet keep_pattern (x : int) = x\n");
  Test.case
    "lower2 formats mutual recursive let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let rec f = g and g = f\n" "let rec f = g\nand g = f\n");
  Test.case
    "lower2 formats local let expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let x = let y = 1 in y\n" "let x = let y = 1 in y\n");
  Test.case
    "lower2 formats function expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let id = fun x -> x\n" "let id = fun x -> x\n");
  Test.case
    "lower2 formats match expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let value = match x with | 0 -> 1 | _ -> 2\n" "let value = match x with | 0 -> 1 | _ -> 2\n");
  Test.case
    "lower2 formats sequence expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let run = first; second\n" "let run = first; second\n");
  Test.case
    "lower2 formats list and array expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let values = [1; 2]\nlet array = [|1; 2|]\n" "let values = [1; 2]\nlet array = [|1; 2|]\n");
  Test.case
    "lower2 preserves parens around function application arguments"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n"
        "let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n");
  Test.case
    "lower2 formats labels and optional labels"
    (fun _ctx -> assert_format2_ml ~expected:"let f ~x ?y = g ~x ?y\n" "let f ~x ?y = g ~x ?y\n");
  Test.case
    "lower2 formats polymorphic variants"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n"
        "let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n");
  Test.case
    "lower2 formats selectors and index expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n"
        "let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n");
  Test.case
    "lower2 formats record expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet { x; y = z; _ } = record\n"
        "let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet { x; y = z; _ } = record\n");
  Test.case
    "lower2 formats binding operator expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = let* x = fetch in let+ y = decode in pair x y\nlet both = let+ x = a and+ y = b in pair x y\n"
        "let value = let* x = fetch in let+ y = decode in pair x y\nlet both = let+ x = a and+ y = b in pair x y\n");
  Test.case
    "lower2 formats local open expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\n"
        "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\n");
  Test.case
    "lower2 formats first-class module expressions"
    (fun _ctx ->
      assert_format2_ml ~expected:"let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\n" "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\n");
  Test.case
    "lower2 formats let module expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\n"
        "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\n");
  Test.case
    "lower2 formats let exception expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n"
        "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n");
  Test.case
    "lower2 formats unreachable expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = match maybe with | Some value -> value | None -> .\n"
        "let value = match maybe with | Some value -> value | None -> .\n");
  Test.case
    "lower2 formats assertion and lazy expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let _ = assert ready\nlet later = lazy compute\n" "let _ = assert ready\nlet later = lazy compute\n");
  Test.case
    "lower2 formats try expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let value = try read () with | Failure -> 0\n" "let value = try read () with | Failure -> 0\n");
  Test.case
    "lower2 formats while and for loops"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let poll = while ready do step () done\nlet up = for i = 0 to n do step i done\nlet down = for i = n downto 0 do step i done\n"
        "let poll = while ready do step () done\nlet up = for i = 0 to n do step i done\nlet down = for i = n downto 0 do step i done\n");
  Test.case
    "lower2 formats lazy exception and interval patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let force = function | lazy value -> value\nlet recovered = match read () with | exception Failure -> 0 | value -> value\nlet classify = function | 'a' .. 'z' -> 1 | _ -> 0\n"
        "let force = function | lazy value -> value\nlet recovered = match read () with | exception Failure -> 0 | value -> value\nlet classify = function | 'a' .. 'z' -> 1 | _ -> 0\n");
  Test.case
    "lower2 adds a final newline"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1\n" "let x = 1");
  Test.case
    "lower2 formats open declarations"
    (fun _ctx -> assert_format2_ml ~expected:"open Foo.Bar\n" "open Foo.Bar\n");
  Test.case
    "lower2 formats simple include external and exception declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"include Foo.Bar\nexternal id: 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n"
        "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n");
  Test.case
    "lower2 formats simple module and module type declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n"
        "module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n");
  Test.case
    "lower2 formats simple signature module declarations"
    (fun _ctx ->
      assert_format2_mli
        ~expected:"module Alias: Foo.S\nmodule Empty: sig end\nmodule type S = Foo.S\nmodule type Abstract\n"
        "module Alias : Foo.S\nmodule Empty : sig end\nmodule type S = Foo.S\nmodule type Abstract\n");
  Test.case
    "lower2 formats type aliases with parameters"
    (fun _ctx -> assert_format2_mli ~expected:"type 'a t = 'a list\n" "type 'a t = 'a list\n");
  Test.case
    "lower2 formats simple value declarations"
    (fun _ctx -> assert_format2_mli ~expected:"val id: 'a -> 'a\n" "val id : 'a -> 'a\n");
  Test.case
    "lower2 rejects unsupported shapes instead of replaying source"
    (fun _ctx -> assert_format2_ml_fails "let object_value = object end\n");
  Test.case
    "lower2 formats a selected existing fixture subset idempotently"
    (fun _ctx -> assert_lower2_existing_fixture_subset ());
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"krasny:lower2" ~tests ~args ())
    ~args:Env.args
    ()
