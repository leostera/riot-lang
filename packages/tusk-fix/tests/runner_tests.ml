open Std

let write_file path content =
  Fs.write content path |> Result.expect ~msg:"failed to write test fixture"

let read_file path =
  Fs.read path |> Result.expect ~msg:"failed to read test fixture"

let run_cli argv =
  match ArgParser.get_matches Tusk_fix.Cli.command argv with
  | Error err -> Error (Failure (ArgParser.error_message err))
  | Ok matches -> Tusk_fix.Cli.run matches

let tests =
  [
    Test.case "no-stdlib rule exposes safe fixes" (fun () ->
        let source =
          "open Stdlib\nlet queue : int Queue.t = Queue.create ()\n"
        in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let fixes =
          List.filter_map Tusk_fix.Diagnostic.fix result.diagnostics
        in
        Test.assert_equal ~expected:3 ~actual:(List.length fixes);
        Ok ());
    Test.case "no-stdlib keeps Unix diagnostic without fix" (fun () ->
        let source = "let home = Unix.getenv \"HOME\"\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let unix_diag =
          List.find_opt
            (fun diag ->
              String.contains (Tusk_fix.Diagnostic.message diag) "Unix")
            result.diagnostics
        in
        match unix_diag with
        | None -> Error "Expected Unix diagnostic"
        | Some diag ->
            Test.assert_equal ~expected:None ~actual:(Tusk_fix.Diagnostic.fix diag);
            Ok ());
    Test.case "no-stdlib skips kernel boundary packages" (fun () ->
        let source = "let home = Unix.getenv \"HOME\"\n" in
        let result =
          Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ())
            ~filename:"packages/kernel/src/fs/file.ml" source
        in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "no-stdlib skips miniriot boundary packages" (fun () ->
        let source = "let home = Unix.getenv \"HOME\"\n" in
        let result =
          Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ())
            ~filename:"packages/miniriot/src/runtime.ml" source
        in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "runner apply rewrites safe modules" (fun () ->
        match
          Fs.with_tempdir ~prefix:"tusk_fix_runner" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file
                "open Stdlib\nlet queue : int Queue.t = Queue.create ()\n";
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Apply file
              in
              Test.assert_true result.changed;
              Test.assert_equal ~expected:0
                ~actual:(List.length result.diagnostics);
              let actual = read_file file in
              let expected =
                "open Std\nlet queue : int Std.Collections.Queue.t = \
                 Std.Collections.Queue.create ()\n"
              in
              Test.assert_equal ~expected ~actual;
              Ok ())
        with
        | Ok result -> result
        | Error _ -> Error "Failed to create tempdir"
        );
    Test.case "check mode reports issues without writing" (fun () ->
        match
          Fs.with_tempdir ~prefix:"tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "open Stdlib\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ())
        with
        | Ok result -> result
        | Error _ -> Error "Failed to create tempdir"
        );
    Test.case "pipeline parses interface files with interface entrypoint" (fun () ->
        let source =
          "type ('request, 'response) t\nval create : unit -> unit\n"
        in
        let result =
          Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ())
            ~filename:"sample.mli" source
        in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.parse_diagnostics);
        Ok ());
    Test.case "scanner skips syn parser corpus inputs" (fun () ->
        match
          Fs.with_tempdir ~prefix:"tusk_fix_scan" (fun tmpdir ->
              let diag_dir = Path.(tmpdir / Path.v "tests" / Path.v "diagnostics") in
              let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
              let generated_dir = Path.(tmpdir / Path.v "tests" / Path.v "generated") in
              let src_dir = Path.(tmpdir / Path.v "src") in
              Fs.create_dir_all diag_dir |> Result.expect ~msg:"mkdir diagnostics";
              Fs.create_dir_all fixtures_dir |> Result.expect ~msg:"mkdir fixtures";
              Fs.create_dir_all generated_dir |> Result.expect ~msg:"mkdir generated";
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file Path.(diag_dir / Path.v "bad.ml") "let =\n";
              write_file Path.(fixtures_dir / Path.v "fixture.ml") "let x = 1\n";
              write_file Path.(generated_dir / Path.v "generated.ml") "let y = 2\n";
              write_file Path.(src_dir / Path.v "real.ml") "let z = 3\n";
              let files =
                Tusk_fix.File_scanner.(scan (create ~root:tmpdir ()))
                |> List.map Path.to_string
                |> List.sort String.compare
              in
              Test.assert_equal ~expected:[ Path.to_string Path.(src_dir / Path.v "real.ml") ]
                ~actual:files;
              Ok ())
        with
        | Ok result -> result
        | Error _ -> Error "Failed to create tempdir"
        );
    Test.case "cli applies safe fixes by default" (fun () ->
        match
          Fs.with_tempdir ~prefix:"tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "open Stdlib\n";
              let result = run_cli [ "fix"; Path.to_string file ] in
              Test.assert_ok result;
              Test.assert_equal ~expected:"open Std\n" ~actual:(read_file file);
              Ok ())
        with
        | Ok result -> result
        | Error _ -> Error "Failed to create tempdir"
        );
    Test.case "cli check exits with error when issues remain" (fun () ->
        match
          Fs.with_tempdir ~prefix:"tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "let home = Unix.getenv \"HOME\"\n";
              let result = run_cli [ "fix"; "--check"; Path.to_string file ] in
              Test.assert_error result;
              Test.assert_equal ~expected:"let home = Unix.getenv \"HOME\"\n"
                ~actual:(read_file file);
              Ok ())
        with
        | Ok result -> result
        | Error _ -> Error "Failed to create tempdir"
        );
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"tusk-fix:runner" ~tests ~args:Env.args)
    ~args:Env.args ()
