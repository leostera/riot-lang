open Std

type input = {
  display_name: string;
  filename: Path.t;
  source: string;
}

let read_stdin = fun () ->
  let buffer = IO.Buffer.create ~size:4_096 in
  match IO.read_to_end (IO.stdin ()) ~into:buffer with
  | Ok _ -> Ok (IO.Buffer.contents buffer)
  | Error err -> Error err

let read_input = fun file ->
  if String.equal file "-" then
    match read_stdin () with
    | Ok source -> Ok { display_name = "stdin"; filename = Path.v "stdin.ml"; source }
    | Error err -> Error ("Error reading stdin: " ^ IO.error_message err)
  else
    match Fs.read (Path.v file) with
    | Ok source -> Ok { display_name = file; filename = Path.v file; source }
    | Error _err -> Error ("Error reading file: " ^ file)

let handle_format = fun file ->
  match read_input file with
  | Error err ->
      Log.error err;
      System.exit 1
  | Ok input ->
      match Krasny.format_source ~filename:input.filename input.source with
      | Ok formatted -> print formatted
      | Error err ->
          Log.error
            ("Error formatting file: "
            ^ input.display_name
            ^ ": "
            ^ Krasny.format_error_to_string err);
          System.exit 1

let handle_syntax_hash = fun file ->
  match read_input file with
  | Error err ->
      Log.error err;
      System.exit 1
  | Ok input -> print (Krasny.syntax_hash_source ~filename:input.filename input.source)

let main ~args =
  let cmd =
    let open ArgParser in
    let open ArgParser.Arg in
    command "krasny"
    |> version "0.1.0"
    |> about "Riot OCaml formatter"
    |> subcommands
      [
        command "format"
        |> about "Format an OCaml source file"
        |> args
          [
            positional "FILE"
            |> help "OCaml source file to format"
            |> required true;
          ];
        command "syntax-hash"
        |> about "Compute a normalized concrete syntax hash"
        |> args
          [
            positional "FILE"
            |> help "OCaml source file to hash"
            |> required true;
          ];
      ]
  in
  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "invalid CLI arguments")
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("format", sub_matches) ->
          let file =
            ArgParser.get_one sub_matches "FILE"
            |> Option.expect ~msg:"FILE required"
          in
          handle_format file;
          Ok ()
      | Some ("syntax-hash", sub_matches) ->
          let file =
            ArgParser.get_one sub_matches "FILE"
            |> Option.expect ~msg:"FILE required"
          in
          handle_syntax_hash file;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          Error (Failure "missing subcommand")

let () = Runtime.run ~main ~args:Env.args ()
