open Std

module Test = Std.Test
module Vector = Std.Collections.Vector

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
  (
    (fun chunk -> chunks := chunk :: !chunks),
    fun () ->
      !chunks
      |> List.reverse
      |> String.concat ""
  )

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create riot-fmt test source slice"

let diagnostics_to_list = fun diagnostics ->
  diagnostics
  |> Vector.to_array
  |> Array.to_list

let parse_jsonl = fun output ->
  output
  |> String.split ~by:"\n"
  |> List.filter ~fn:(fun line -> not (String.equal line ""))
  |> List.map
    ~fn:(fun line ->
      Data.Json.from_string line
      |> Result.expect ~msg:"parse json line")

let parse_package_manifest = fun ~path ~relative_path toml ->
  Data.Toml.parse toml
  |> Result.expect ~msg:"parse package manifest toml"
  |> Riot_model.Package_manifest.from_toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path
    ~relative_path
  |> Result.expect ~msg:"package manifest"

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
  with_tempdir
    "riot_fmt_explicit_file"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      let untouched = Path.(tmpdir / Path.v "untouched.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      Fs.write "let y = 3 + 4\nlet g y = y + 1\n" untouched
      |> Result.expect ~msg:"write untouched";
      let matches =
        parse_fmt [ "fmt"; Path.to_string needs ]
        |> Result.expect ~msg:"parse fmt args"
      in
      Riot_fmt.run matches
      |> Result.expect ~msg:"format explicit target";
      let formatted =
        Fs.read needs
        |> Result.expect ~msg:"read formatted file"
      in
      let untouched_source =
        Fs.read untouched
        |> Result.expect ~msg:"read untouched file"
      in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted;
      Test.assert_equal ~expected:"let y = 3 + 4\nlet g y = y + 1\n" ~actual:untouched_source;
      Ok ())

let test_fmt_is_quiet_by_default = fun _ctx ->
  with_tempdir
    "riot_fmt_quiet"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      let matches =
        parse_fmt [ "fmt"; Path.to_string needs ]
        |> Result.expect ~msg:"parse fmt args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      Riot_fmt.run ~stdout ~stderr matches
      |> Result.expect ~msg:"format explicit target quietly";
      let formatted =
        Fs.read needs
        |> Result.expect ~msg:"read formatted file"
      in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted;
      Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
      Ok ())

let test_fmt_check_is_quiet_when_all_files_are_formatted = fun _ctx ->
  with_tempdir
    "riot_fmt_check_quiet"
    (fun tmpdir ->
      let formatted = Path.(tmpdir / Path.v "formatted.ml") in
      Fs.write "let x = 1 + 2\n\nlet f x = x + 1\n" formatted
      |> Result.expect ~msg:"write formatted";
      let matches =
        parse_fmt [ "fmt"; "--check"; Path.to_string formatted ]
        |> Result.expect ~msg:"parse fmt check args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      Riot_fmt.run ~stdout ~stderr matches
      |> Result.expect ~msg:"check formatted target quietly";
      Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
      Ok ())

let test_fmt_check_reports_files_that_need_formatting = fun _ctx ->
  with_tempdir
    "riot_fmt_check_reports"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
      |> Result.expect ~msg:"write needs";
      let matches =
        parse_fmt [ "fmt"; "--check"; Path.to_string needs ]
        |> Result.expect ~msg:"parse fmt check args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      (
        match Riot_fmt.run ~stdout ~stderr matches with
        | Ok () -> Error "expected fmt --check to fail when a file needs formatting"
        | Error _ ->
            if
              String.contains (stdout_contents ()) "needs.ml (needs formatting)"
              && String.equal (stderr_contents ()) ""
            then
              Ok ()
            else
              Error ("expected fmt --check to report the unformatted file, got stdout="
              ^ stdout_contents ()
              ^ " stderr="
              ^ stderr_contents ())
      ))

let test_fmt_prints_syn_diagnostics_for_syntax_errors = fun _ctx ->
  with_tempdir
    "riot_fmt_syntax_error"
    (fun tmpdir ->
      let broken = Path.(tmpdir / Path.v "broken.ml") in
      let source = "let x =\n" in
      Fs.write source broken
      |> Result.expect ~msg:"write broken";
      let matches =
        parse_fmt [ "fmt"; Path.to_string broken ]
        |> Result.expect ~msg:"parse fmt args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      let parsed = Syn.parse ~filename:broken (source_slice source) in
      if Vector.length parsed.diagnostics = 0 then
        Error "expected broken source to produce syn diagnostics"
      else
        let expected =
          Syn.DiagnosticReporter.format
            ~file:(Path.to_string broken)
            ~source
            (diagnostics_to_list parsed.diagnostics)
        in
        (
          match Riot_fmt.run ~stdout ~stderr matches with
          | Ok () -> Error "expected syntax error formatting to fail"
          | Error _ ->
              Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
              Test.assert_equal ~expected ~actual:(stderr_contents ());
              Ok ()
        ))

let test_fmt_check_prints_syn_diagnostics_for_syntax_errors = fun _ctx ->
  with_tempdir
    "riot_fmt_check_syntax_error"
    (fun tmpdir ->
      let broken = Path.(tmpdir / Path.v "broken.ml") in
      let source = "let x =\n" in
      Fs.write source broken
      |> Result.expect ~msg:"write broken";
      let matches =
        parse_fmt [ "fmt"; "--check"; Path.to_string broken ]
        |> Result.expect ~msg:"parse fmt check args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      let parsed = Syn.parse ~filename:broken (source_slice source) in
      if Vector.length parsed.diagnostics = 0 then
        Error "expected broken source to produce syn diagnostics"
      else
        let expected =
          Syn.DiagnosticReporter.format
            ~file:(Path.to_string broken)
            ~source
            (diagnostics_to_list parsed.diagnostics)
        in
        (
          match Riot_fmt.run ~stdout ~stderr matches with
          | Ok () -> Error "expected syntax error fmt --check to fail"
          | Error _ ->
              Test.assert_equal ~expected ~actual:(stdout_contents ());
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_fmt_json_includes_structured_syn_diagnostics_for_syntax_errors = fun _ctx ->
  with_tempdir
    "riot_fmt_json_syntax_error"
    (fun tmpdir ->
      let broken = Path.(tmpdir / Path.v "broken.ml") in
      let source = "let x =\n" in
      Fs.write source broken
      |> Result.expect ~msg:"write broken";
      let matches =
        parse_fmt [ "fmt"; "--json"; Path.to_string broken ]
        |> Result.expect ~msg:"parse fmt json args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      let parsed = Syn.parse ~filename:broken (source_slice source) in
      if Vector.length parsed.diagnostics = 0 then
        Error "expected broken source to produce syn diagnostics"
      else
        (
          match Riot_fmt.run ~stdout ~stderr matches with
          | Ok () -> Error "expected syntax error fmt --json to fail"
          | Error _ ->
              let file_event =
                stdout_contents ()
                |> parse_jsonl
                |> List.find
                  ~fn:(fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "file") -> true
                    | _ -> false)
                |> Option.expect ~msg:"file event missing"
              in
              let expected = Some (
                Data.Json.Array (
                  parsed.diagnostics
                  |> diagnostics_to_list
                  |> List.map ~fn:Syn.Diagnostic.to_json
                )
              )
              in
              Test.assert_equal ~expected ~actual:(Data.Json.get_field "diagnostics" file_event);
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ()
        ))

let test_fmt_explain_prints_syn_explanation = fun _ctx ->
  let matches =
    parse_fmt [ "fmt"; "--explain"; "E0001" ]
    |> Result.expect ~msg:"parse fmt explain args"
  in
  let (stdout, stdout_contents) = make_capture_writer () in
  let (stderr, stderr_contents) = make_capture_writer () in
  Riot_fmt.run ~stdout ~stderr matches
  |> Result.expect ~msg:"explain error id";
  Test.assert_equal
    ~expected:(Syn.Error.explain Syn.Error.E0001_MalformedTypeVariable ^ "\n")
    ~actual:(stdout_contents ());
  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
  Ok ()

let test_fmt_explain_rejects_unknown_error_id = fun _ctx ->
  let matches =
    parse_fmt [ "fmt"; "--explain"; "E9999" ]
    |> Result.expect ~msg:"parse fmt explain args"
  in
  let (stdout, stdout_contents) = make_capture_writer () in
  let (stderr, stderr_contents) = make_capture_writer () in
  (
    match Riot_fmt.run ~stdout ~stderr matches with
    | Ok () -> Error "expected unknown explain id to fail"
    | Error _ ->
        Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
        Test.assert_equal ~expected:"Unknown error code: E9999\n" ~actual:(stderr_contents ());
        Ok ()
  )

let test_fmt_workspace_scan_ignores_external_packages = fun _ctx ->
  with_tempdir
    "riot_fmt_workspace_scan"
    (fun tmpdir ->
      let workspace_root = tmpdir in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let package_src = Path.(package_root / Path.v "src") in
      let external_root = Path.(workspace_root / Path.v ".external/dep") in
      let external_tests = Path.(external_root / Path.v "tests/diagnostics") in
      Fs.create_dir_all package_src
      |> Result.expect ~msg:"create package src";
      Fs.create_dir_all external_tests
      |> Result.expect ~msg:"create external tests";
      Fs.write
        "[workspace]\nmembers = [\"packages/demo\"]\n"
        Path.(workspace_root / Path.v "riot.toml")
      |> Result.expect ~msg:"write workspace manifest";
      Fs.write
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n"
        Path.(package_root / Path.v "riot.toml")
      |> Result.expect ~msg:"write package manifest";
      Fs.write "let x = 1 + 2\n\nlet f x = x + 1\n" Path.(package_src / Path.v "demo.ml")
      |> Result.expect ~msg:"write local source";
      Fs.write
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n"
        Path.(external_root / Path.v "riot.toml")
      |> Result.expect ~msg:"write external manifest";
      Fs.write "let x as = 5\n" Path.(external_tests / Path.v "broken.ml")
      |> Result.expect ~msg:"write external broken file";
      let workspace =
        Riot_model.Workspace.make
          ~root:workspace_root
          ~packages:[
            parse_package_manifest
              ~path:package_root
              ~relative_path:(Path.v "packages/demo")
              "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n";
            parse_package_manifest
              ~path:external_root
              ~relative_path:external_root
              "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n";
          ]
          ()
      in
      let matches =
        parse_fmt [ "fmt" ]
        |> Result.expect ~msg:"parse fmt args"
      in
      let (stdout, stdout_contents) = make_capture_writer () in
      let (stderr, stderr_contents) = make_capture_writer () in
      Riot_fmt.run ~workspace ~stdout ~stderr matches
      |> Result.expect ~msg:"workspace fmt should ignore external packages";
      Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
      Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
      Ok ())

let tests =
  Test.[
    case "fmt: accept multiple path arguments" test_fmt_accepts_multiple_paths;
    case "fmt: usage shows variadic paths" test_fmt_usage_shows_variadic_paths;
    case "fmt: accept explain option" test_fmt_accepts_explain_option;
    case "fmt: format rewrites only the explicit file" test_fmt_formats_only_explicit_file;
    case "fmt: default format is quiet on success" test_fmt_is_quiet_by_default;
    case "fmt: check is quiet on success" test_fmt_check_is_quiet_when_all_files_are_formatted;
    case
      "fmt: check reports files needing formatting"
      test_fmt_check_reports_files_that_need_formatting;
    case
      "fmt: syntax errors render syn diagnostics"
      test_fmt_prints_syn_diagnostics_for_syntax_errors;
    case
      "fmt: check syntax errors render syn diagnostics"
      test_fmt_check_prints_syn_diagnostics_for_syntax_errors;
    case
      "fmt: json syntax errors include structured syn diagnostics"
      test_fmt_json_includes_structured_syn_diagnostics_for_syntax_errors;
    case "fmt: explain prints syn explanation" test_fmt_explain_prints_syn_explanation;
    case "fmt: explain rejects unknown error id" test_fmt_explain_rejects_unknown_error_id;
    case
      "fmt: workspace scan ignores external packages"
      test_fmt_workspace_scan_ignores_external_packages;
  ]

let name = "Riot Fmt Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
