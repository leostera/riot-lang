open Std

module Test = Std.Test

let parse_cli = fun args ->
  match ArgParser.get_matches (Riot_cli.Cli.build_cli ()) args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_lsp_stdio_subcommand_parses = fun _ctx ->
  match parse_cli [ "riot"; "lsp"; "stdio" ] with
  | Error err -> Error ("expected lsp stdio args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("lsp", lsp_matches) ->
          match ArgParser.get_subcommand lsp_matches with
          | Some ("stdio", _) -> Ok ()
          | Some (name, _) -> Error ("expected stdio transport, got: " ^ name)
          | None -> Error "expected lsp transport subcommand"
      | Some (name, _) -> Error ("expected lsp command, got: " ^ name)
      | None -> Error "expected top-level subcommand"

let test_help_normalizes_to_global_help = fun _ctx ->
  Test.assert_equal
    ~expected:[ "riot"; "--help" ]
    ~actual:(Riot_cli.Cli.normalize_args [ "riot"; "help" ]);
  Ok ()

let test_build_package_named_lsp_parses = fun _ctx ->
  match parse_cli [ "riot"; "build"; "-p"; "lsp"; ] with
  | Error err -> Error ("expected build lsp args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("build", build_matches) ->
          Test.assert_equal ~expected:[ "lsp" ] ~actual:(ArgParser.get_many build_matches "package");
          Ok ()
      | Some (name, _) -> Error ("expected build command, got: " ^ name)
      | None -> Error "expected top-level subcommand"

let tests =
  Test.[
    case "lsp: parse stdio transport subcommand" test_lsp_stdio_subcommand_parses;
    case "cli: help normalizes to global help" test_help_normalizes_to_global_help;
    case "lsp: build package named lsp parses normally" test_build_package_named_lsp_parses;
  ]

let name = "Riot CLI LSP Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
