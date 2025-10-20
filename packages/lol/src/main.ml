open Lol
open Std

let () =
  Log.set_level Log.Info;

  let to_json_stream_cmd =
    ArgParser.command "to-json-stream"
    |> ArgParser.about "Convert CSV to JSON stream (one JSON object per line)"
    |> ArgParser.arg
         (ArgParser.Arg.positional "input"
         |> ArgParser.Arg.required true
         |> ArgParser.Arg.help "Input CSV file")
  in

  let csv_cmd =
    ArgParser.command "csv"
    |> ArgParser.about "CSV utilities"
    |> ArgParser.subcommands [ to_json_stream_cmd ]
  in

  let main_cmd =
    ArgParser.command "lol"
    |> ArgParser.about "Little utilities for testing and debugging"
    |> ArgParser.version "0.1.0"
    |> ArgParser.subcommands [ csv_cmd ]
  in

  match ArgParser.get_matches main_cmd Env.args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help main_cmd
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("csv", csv_matches) -> (
          match ArgParser.get_subcommand csv_matches with
          | Some ("to-json-stream", sub_matches) -> (
              let input = ArgParser.get_path sub_matches "input" in
              match input with
              | Some path -> Csv_cmd.to_json_stream path
              | None ->
                  Log.error "Input file required";
                  ArgParser.print_help to_json_stream_cmd)
          | _ -> ArgParser.print_help csv_cmd)
      | _ -> ArgParser.print_help main_cmd)
