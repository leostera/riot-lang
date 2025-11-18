open Std

let cli =
  let open ArgParser in
  command "poneglyph"
  |> version "0.1.0"
  |> about "Graph database CLI"
  |> subcommands [
      Poneglyph.Cli.New.command;
      Poneglyph.Cli.State.command;
      Poneglyph.Cli.Load.command;
      Poneglyph.Cli.Query.command;
      Poneglyph.Cli.Get.command;
      Poneglyph.Cli.Stats.command;
      Poneglyph.Cli.Compact.command;
      Poneglyph.Cli.Inspect.Sstable.command;
      Poneglyph.Cli.Inspect.Index.command;
      Poneglyph.Cli.Search.command;
      Poneglyph.Cli.Dump.command;
    ]

let main ~args =
  match ArgParser.get_matches cli args with
  | Error err ->
      ArgParser.print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> 
      match ArgParser.get_subcommand matches with
      | Some ("new", sub_matches) -> Poneglyph.Cli.New.run sub_matches
      | Some ("state", sub_matches) -> Poneglyph.Cli.State.run sub_matches
      | Some ("load", sub_matches) -> Poneglyph.Cli.Load.run sub_matches
      | Some ("query", sub_matches) -> Poneglyph.Cli.Query.run sub_matches
      | Some ("get", sub_matches) -> Poneglyph.Cli.Get.run sub_matches
      | Some ("stats", sub_matches) -> Poneglyph.Cli.Stats.run sub_matches
      | Some ("compact", sub_matches) -> Poneglyph.Cli.Compact.run sub_matches
      | Some ("inspect-sstable", sub_matches) -> Poneglyph.Cli.Inspect.Sstable.run sub_matches
      | Some ("inspect-index", sub_matches) -> Poneglyph.Cli.Inspect.Index.run sub_matches
      | Some ("search", sub_matches) -> Poneglyph.Cli.Search.run sub_matches
      | Some ("dump", sub_matches) -> Poneglyph.Cli.Dump.run sub_matches
      | _ -> (ArgParser.print_help cli; Ok ())
    

let () =
  Miniriot.run ~main ~args:Env.args ()
