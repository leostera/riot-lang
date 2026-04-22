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

let tests = [
  Test.case
    "lower2 keeps empty implementations empty"
    (fun _ctx -> assert_format2_ml ~expected:"" "");
  Test.case
    "lower2 formats simple let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1 + 2\n" "let x = 1 + 2\n");
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
    (fun _ctx -> assert_format2_ml_fails "let xs = [1]\n");
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"krasny:lower2" ~tests ~args ())
    ~args:Env.args
    ()
