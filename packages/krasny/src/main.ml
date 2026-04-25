open Std

let parse_file = fun ~file ~source ->
  if String.ends_with ~suffix:".mli" file then
    Syn.parse_interface source
  else
    Syn.parse_implementation source

let handle_format = fun file ->
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file: " ^ file);
      System.exit 1
  | Ok source ->
      let result = parse_file ~file ~source in
      match Krasny.format result with
      | Ok formatted -> print formatted
      | Error _err ->
          Log.error ("Error formatting file without a CST: " ^ file);
          System.exit 1

let handle_syntax_hash = fun file ->
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file: " ^ file);
      System.exit 1
  | Ok source ->
      let result = parse_file ~file ~source in
      print (Krasny.syntax_hash result)

let main ~args =
  let cmd =
    let open ArgParser in
      let open ArgParser.Arg in command "krasny"
      |> version "0.1.0"
      |> about "Riot OCaml formatter"
      |> subcommands
        [
          command "format"
          |> about "Format an OCaml source file"
          |> args [ positional "FILE" |> help "OCaml source file to format" |> required true; ];
          command "syntax-hash"
          |> about "Compute a normalized concrete syntax hash"
          |> args [ positional "FILE" |> help "OCaml source file to hash" |> required true; ];
        ]
  in
  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "invalid CLI arguments")
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("format", sub_matches) ->
          let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
          handle_format file;
          Ok ()
      | Some ("syntax-hash", sub_matches) ->
          let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
          handle_syntax_hash file;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          Error (Failure "missing subcommand")
    )

let () = Runtime.run ~main ~args:Env.args ()
