open Std

let write_file path content =
  Fs.write content path |> Result.expect ~msg:"failed to write test fixture"

let read_file path =
  Fs.read path |> Result.expect ~msg:"failed to read test fixture"

let run_cli argv =
  match ArgParser.get_matches Tusk_fix.Cli.command ("fix" :: argv) with
  | Error err -> Error (Failure (ArgParser.error_message err))
  | Ok matches -> Tusk_fix.Cli.run matches

let with_tempdir prefix fn =
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let tests =
  [
    Test.case "no-stdlib rule exposes safe fixes" (fun () ->
        let source = "open Stdlib\nlet cmp = Stdlib.compare\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let fixes =
          List.filter_map Tusk_fix.Diagnostic.fix result.diagnostics
        in
        Test.assert_equal ~expected:2 ~actual:(List.length fixes);
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
    Test.case "no-stdlib emits stable module-specific codes" (fun () ->
        let source = "open Unix\nlet cmp = Stdlib.compare\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes =
          result.diagnostics
          |> List.filter_map Tusk_fix.Diagnostic.code
          |> List.map Tusk_fix.Diagnostic_code.to_id
          |> List.sort String.compare
        in
        Test.assert_equal ~expected:[ "F0001"; "F0003" ] ~actual:codes;
        Ok ());
    Test.case "diagnostic code registry explains Unix violations" (fun () ->
        match Tusk_fix.Diagnostic_code.explain "F0001" with
        | None -> Error "Expected explanation for F0001"
        | Some entry ->
            Test.assert_equal ~expected:"F0001"
              ~actual:(Tusk_fix.Diagnostic_code.to_id entry.code);
            Test.assert_true
              (String.contains entry.body "scheduler");
            Ok ());
    Test.case "no-stdlib ignores non-stdlib Queue modules" (fun () ->
        let source =
          "val queue : 'value t -> 'value Collections.Queue.t t\nlet q = Queue.create ()\n"
        in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "package rule override disables no-stdlib locally" (fun () ->
        with_tempdir "tusk_fix_config" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "kernel") in
              let package_toml = Path.(package_dir / Path.v "tusk.toml") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/kernel\"]\n\n[tusk.fix]\nrules = [\"no-stdlib\"]\n";
              write_file package_toml
                "[package]\nname = \"kernel\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-no-stdlib\"]\n\n[lib]\npath = \"src/kernel.ml\"\n";
              write_file file "let home = Unix.getenv \"HOME\"\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              let pipeline =
                Tusk_fix.Config.pipeline_for_file (Some scope) file
              in
              let result =
                Tusk_fix.Pipeline.run pipeline
                  ~filename:(Path.to_string file)
                  "let home = Unix.getenv \"HOME\"\n"
              in
              Test.assert_equal ~expected:0
                ~actual:(List.length result.diagnostics);
              Ok ()));
    Test.case "workspace ignore patterns exclude matching files" (fun () ->
        with_tempdir "tusk_fix_ignore" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let ignored = Path.(src_dir / Path.v "ignored.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nignore = [\"ignored.ml\"]\nrules = [\"no-stdlib\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file ignored "let home = Unix.getenv \"HOME\"\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              Test.assert_true (Tusk_fix.Config.should_ignore_file (Some scope) ignored);
              Ok ()));
    Test.case "config shorthand enables and disables rules" (fun () ->
        with_tempdir "tusk_fix_rules" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [\"no-stdlib\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-no-stdlib\"]\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file file "let home = Unix.getenv \"HOME\"\n";
              let result =
                Tusk_fix.Runner.run_files
                  ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file (Tusk_fix.Config.load_scope ~cwd:tmpdir))
                  ~mode:Tusk_fix.Runner.Check [ file ]
              in
              Test.assert_equal ~expected:0
                ~actual:result.summary.remaining_diagnostics;
              Ok ()));
    Test.case "config table uses explicit rule state" (fun () ->
        with_tempdir "tusk_fix_rule_state" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [{ name = \"no-stdlib\", state = \"enabled\" }]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [{ name = \"no-stdlib\", state = \"disabled\" }]\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file file "let home = Unix.getenv \"HOME\"\n";
              let result =
                Tusk_fix.Runner.run_files
                  ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file (Tusk_fix.Config.load_scope ~cwd:tmpdir))
                  ~mode:Tusk_fix.Runner.Check [ file ]
              in
              Test.assert_equal ~expected:0
                ~actual:result.summary.remaining_diagnostics;
              Ok ()));
    Test.case "runner apply rewrites only direct Stdlib usage" (fun () ->
        with_tempdir "tusk_fix_runner" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file
                "open Stdlib\nlet cmp = Stdlib.compare\n";
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Apply file
              in
              Test.assert_true result.changed;
              Test.assert_equal ~expected:0
                ~actual:(List.length result.diagnostics);
              let actual = read_file file in
              let expected =
                "open Std\nlet cmp = Std.compare\n"
              in
              Test.assert_equal ~expected ~actual;
              Ok ()));
    Test.case "check mode reports Unix issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "let home = Unix.getenv \"HOME\"\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "cli applies safe fixes by default" (fun () ->
        with_tempdir "tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "open Stdlib\n";
              let result = run_cli [ Path.to_string file ] in
              Test.assert_ok result;
              Test.assert_equal ~expected:"open Std\n" ~actual:(read_file file);
              Ok ()));
    Test.case "cli check exits with error when issues remain" (fun () ->
        with_tempdir "tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "let home = Unix.getenv \"HOME\"\n";
              let result = run_cli [ "--check"; Path.to_string file ] in
              Test.assert_error result;
              Test.assert_equal ~expected:"let home = Unix.getenv \"HOME\"\n"
                ~actual:(read_file file);
              Ok ()));
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
        with_tempdir "tusk_fix_scan" (fun tmpdir ->
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
              Ok ()));
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"tusk-fix:runner" ~tests ~args:Env.args)
    ~args:Env.args ()
