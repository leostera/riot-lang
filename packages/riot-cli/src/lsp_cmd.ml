open Std

let stdio_command =
  let open ArgParser in
    command "stdio" |> about "Start Riot LSP server over stdio"

let command =
  let open ArgParser in
    command "lsp"
    |> about "Start Riot LSP server"
    |> subcommands [ stdio_command ]

let run = fun matches ->
  match ArgParser.get_subcommand matches with
  | Some ("stdio", _) -> Riot_lsp.run ()
  | Some (transport, _) ->
      ArgParser.print_error (ArgParser.UnknownSubcommand transport);
      Error (Failure ("Unknown lsp transport: " ^ transport))
  | None ->
      ArgParser.print_help command;
      Ok ()
