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
  Test.assert_equal ~expected ~actual;
  Ok ()

let assert_format2_mli = fun ~expected source ->
  let actual = format2_mli source |> Result.expect ~msg:"interface should format through lower2" in
  Test.assert_equal ~expected ~actual;
  Ok ()

let assert_format2_ml_fails = fun source ->
  match format2_ml source with
  | Ok formatted -> Error ("lower2 unexpectedly formatted unsupported source as:\n" ^ formatted)
  | Error _ -> Ok ()

let assert_lower2_fixture_idempotent = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  match format2_source ~filename:path source with
  | Error err ->
      Error (Path.to_string path ^ " failed lower2 formatting: " ^ Krasny.format_error_to_string err)
  | Ok formatted -> (
      match format2_source ~filename:path formatted with
      | Error err ->
          Error
            (Path.to_string path
            ^ " formatted once but failed to format again: "
            ^ Krasny.format_error_to_string err)
      | Ok reformatted ->
          Test.assert_equal ~expected:formatted ~actual:reformatted;
          Ok ()
    )

let assert_lower2_existing_fixture_subset = fun () ->
  let fixtures = [
    Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml";
    Path.v "packages/krasny/tests/fixtures/0415_nested_fun_parameter_stability.ml";
    Path.v "packages/krasny/tests/fixtures/0952_multiline_list_expression_no_trailing_separator.ml";
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
    "lower2 formats local let expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let x = let y = 1 in y\n" "let x = let y = 1 in y\n");
  Test.case
    "lower2 formats function expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let id = fun x -> x\n" "let id = fun x -> x\n");
  Test.case
    "lower2 formats match expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value = match x with | 0 -> 1 | _ -> 2\n"
        "let value = match x with | 0 -> 1 | _ -> 2\n");
  Test.case
    "lower2 formats sequence expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let run = first; second\n" "let run = first; second\n");
  Test.case
    "lower2 formats list and array expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let values = [1; 2]\nlet array = [|1; 2|]\n"
        "let values = [1; 2]\nlet array = [|1; 2|]\n");
  Test.case
    "lower2 adds a final newline"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1\n" "let x = 1");
  Test.case
    "lower2 formats open declarations"
    (fun _ctx -> assert_format2_ml ~expected:"open Foo.Bar\n" "open Foo.Bar\n");
  Test.case
    "lower2 formats type aliases with parameters"
    (fun _ctx -> assert_format2_mli ~expected:"type 'a t = 'a list\n" "type 'a t = 'a list\n");
  Test.case
    "lower2 formats simple value declarations"
    (fun _ctx -> assert_format2_mli ~expected:"val id: 'a -> 'a\n" "val id : 'a -> 'a\n");
  Test.case
    "lower2 rejects unsupported shapes instead of replaying source"
    (fun _ctx -> assert_format2_ml_fails "let record = { x = 1 }\n");
  Test.case
    "lower2 formats a selected existing fixture subset idempotently"
    (fun _ctx -> assert_lower2_existing_fixture_subset ());
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"krasny:lower2" ~tests ~args ())
    ~args:Env.args
    ()
