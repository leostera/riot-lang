open Std
module Test = Std.Test

let parse_cli = fun args ->
  match ArgParser.get_matches (Riot_cli.Cli.build_cli ()) args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_lsp_stdio_subcommand_parses = fun _ctx ->
  match parse_cli [ "riot"; "lsp"; "stdio" ] with
  | Error err -> Error ("expected lsp stdio args to parse: " ^ err)
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("lsp", lsp_matches) -> (
          match ArgParser.get_subcommand lsp_matches with
          | Some ("stdio", _) -> Ok ()
          | Some (name, _) -> Error ("expected stdio transport, got: " ^ name)
          | None -> Error "expected lsp transport subcommand"
        )
      | Some (name, _) -> Error ("expected lsp command, got: " ^ name)
      | None -> Error "expected top-level subcommand"
    )

let test_normalize_args_aliases_bare_lsp_to_stdio = fun _ctx ->
  Test.assert_equal
    ~expected:[ "riot"; "lsp"; "stdio" ]
    ~actual:(Riot_cli.Cli.normalize_args [ "riot"; "lsp" ]);
  Ok ()

let test_normalize_args_keeps_explicit_transport = fun _ctx ->
  Test.assert_equal
    ~expected:[ "riot"; "lsp"; "stdio" ]
    ~actual:(Riot_cli.Cli.normalize_args [ "riot"; "lsp"; "stdio" ]);
  Ok ()

let test_normalize_args_keeps_non_lsp_commands = fun _ctx ->
  Test.assert_equal
    ~expected:[ "riot"; "build"; "syn" ]
    ~actual:(Riot_cli.Cli.normalize_args [ "riot"; "build"; "syn" ]);
  Ok ()

let tests =
  Test.[
    case "lsp: parse stdio transport subcommand" test_lsp_stdio_subcommand_parses;
    case "lsp: bare lsp aliases to stdio" test_normalize_args_aliases_bare_lsp_to_stdio;
    case "lsp: explicit transport is preserved" test_normalize_args_keeps_explicit_transport;
    case "lsp: other commands are preserved" test_normalize_args_keeps_non_lsp_commands;
  ]

let name = "Riot CLI LSP Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
