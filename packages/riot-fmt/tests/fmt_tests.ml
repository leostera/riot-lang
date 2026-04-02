open Std
module Test = Std.Test

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let parse_fmt = fun args ->
  match ArgParser.get_matches Riot_fmt.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let make_capture_writer = fun () ->
  let chunks = ref [] in
  ((fun chunk -> chunks := chunk :: !chunks), fun () -> !chunks |> List.rev |> String.concat "")

let test_fmt_accepts_multiple_paths = fun _ctx ->
  match parse_fmt [ "fmt"; "packages/blink/src/connection.ml"; "packages/syn/src/parser.ml" ] with
  | Error err -> Error ("expected fmt args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "path" in
      Test.assert_equal
        ~expected:[ "packages/blink/src/connection.ml"; "packages/syn/src/parser.ml" ]
        ~actual;
      Ok ()

let test_fmt_usage_shows_variadic_paths = fun _ctx ->
  let usage = ArgParser.usage_string Riot_fmt.command in
  if String.contains usage "path..." then
    Ok ()
  else
    Error ("expected variadic path usage, got: " ^ usage)

let test_fmt_accepts_explain_option = fun _ctx ->
  match parse_fmt [ "fmt"; "--explain"; "E0001" ] with
  | Error err -> Error ("expected fmt explain args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "E0001") ~actual:(ArgParser.get_one matches "explain");
      Ok ()

let test_fmt_formats_only_explicit_file = fun _ctx ->
  with_tempdir "riot_fmt_explicit_file"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      let untouched = Path.(tmpdir / Path.v "untouched.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
      Fs.write "let y = 3 + 4\nlet g y = y + 1\n" untouched |> Result.expect ~msg:"write untouched";
      let matches = parse_fmt [ "fmt"; Path.to_string needs ] |> Result.expect ~msg:"parse fmt args" in
      Riot_fmt.run matches |> Result.expect ~msg:"format explicit target";
      let formatted = Fs.read needs |> Result.expect ~msg:"read formatted file" in
      let untouched_source = Fs.read untouched |> Result.expect ~msg:"read untouched file" in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted;
      Test.assert_equal ~expected:"let y = 3 + 4\nlet g y = y + 1\n" ~actual:untouched_source;
      Ok ())

let test_fmt_is_quiet_by_default = fun _ctx ->
  with_tempdir "riot_fmt_quiet"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
      let matches = parse_fmt [ "fmt"; Path.to_string needs ] |> Result.expect ~msg:"parse fmt args" in
      let stdout, stdout_contents = make_capture_writer () in
      let stderr, stderr_contents = make_capture_writer () in
      Riot_fmt.run ~stdout ~stderr matches |> Result.expect ~msg:"format explicit target quietly";
      let formatted = Fs.read needs |> Result.expect ~msg:"read formatted file" in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted;
      Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
      Ok ())

let test_fmt_prints_syn_diagnostics_for_syntax_errors = fun _ctx ->
  with_tempdir "riot_fmt_syntax_error"
    (fun tmpdir ->
      let broken = Path.(tmpdir / Path.v "broken.ml") in
      let source = "let x =\n" in
      Fs.write source broken |> Result.expect ~msg:"write broken";
      let matches = parse_fmt [ "fmt"; Path.to_string broken ] |> Result.expect ~msg:"parse fmt args" in
      let stdout, stdout_contents = make_capture_writer () in
      let stderr, stderr_contents = make_capture_writer () in
      let parsed = Syn.parse ~filename:broken source in
      if List.is_empty parsed.diagnostics then
        Error "expected broken source to produce syn diagnostics"
      else
        let expected =
          Syn.DiagnosticReporter.format
            ~file:(Path.to_string broken)
            ~source
            parsed.diagnostics
        in
        (
          match Riot_fmt.run ~stdout ~stderr matches with
          | Ok () -> Error "expected syntax error formatting to fail"
          | Error _ ->
              Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
              Test.assert_equal ~expected ~actual:(stderr_contents ());
              Ok ()
        ))

let test_fmt_explain_prints_syn_explanation = fun _ctx ->
  let matches = parse_fmt [ "fmt"; "--explain"; "E0001" ] |> Result.expect ~msg:"parse fmt explain args" in
  let stdout, stdout_contents = make_capture_writer () in
  let stderr, stderr_contents = make_capture_writer () in
  Riot_fmt.run ~stdout ~stderr matches |> Result.expect ~msg:"explain error id";
  Test.assert_equal
    ~expected:(Syn.Error.explain Syn.Error.E0001_MalformedTypeVariable ^ "\n")
    ~actual:(stdout_contents ());
  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
  Ok ()

let test_fmt_explain_rejects_unknown_error_id = fun _ctx ->
  let matches = parse_fmt [ "fmt"; "--explain"; "E9999" ] |> Result.expect ~msg:"parse fmt explain args" in
  let stdout, stdout_contents = make_capture_writer () in
  let stderr, stderr_contents = make_capture_writer () in
  (
    match Riot_fmt.run ~stdout ~stderr matches with
    | Ok () -> Error "expected unknown explain id to fail"
    | Error _ ->
        Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
        Test.assert_equal ~expected:"Unknown error code: E9999\n" ~actual:(stderr_contents ());
        Ok ()
  )

let tests =
  Test.[
    case "fmt: accept multiple path arguments" test_fmt_accepts_multiple_paths;
    case "fmt: usage shows variadic paths" test_fmt_usage_shows_variadic_paths;
    case "fmt: accept explain option" test_fmt_accepts_explain_option;
    case "fmt: format rewrites only the explicit file" test_fmt_formats_only_explicit_file;
    case "fmt: default format is quiet on success" test_fmt_is_quiet_by_default;
    case "fmt: syntax errors render syn diagnostics" test_fmt_prints_syn_diagnostics_for_syntax_errors;
    case "fmt: explain prints syn explanation" test_fmt_explain_prints_syn_explanation;
    case "fmt: explain rejects unknown error id" test_fmt_explain_rejects_unknown_error_id;
  ]

let name = "Riot Fmt Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
