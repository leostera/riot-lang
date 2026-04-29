open Std

let command =
  let open ArgParser in
  command "lsp"
  |> about "Language Server Protocol entrypoint"
  |> subcommands
    [
      command "stdio"
      |> about "Run the language server over stdio";
    ]

let run = fun matches ->
  let open ArgParser in
  match get_subcommand matches with
  | Some ("stdio", _) -> Riot_lsp.run ()
  | _ ->
      eprintln "Usage: riot lsp stdio";
      Error (Failure "Unknown lsp subcommand")
