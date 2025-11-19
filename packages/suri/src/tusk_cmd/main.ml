open Std

let name = "suri"

let command =
  let open ArgParser in
  let open Arg in
  command name
  |> about "Suri web framework commands"
  |> subcommands [
      Serve.Serve.command;
    ]

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      Error (Failure "Argument parsing failed")
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("serve", serve_matches) -> Serve.Serve.run serve_matches
      | None ->
          ArgParser.print_error (ArgParser.MissingSubcommand "Expected subcommand (serve)");
          Error (Failure "Missing subcommand")
      | Some (cmd, _) ->
          ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
          Error (Failure ("Unknown subcommand: " ^ cmd))
  )

external array_to_list : 'a array -> 'a list = "%array_to_list"

let () = 
  let open struct external argv : unit -> string array = "caml_sys_argv" end in
  let args = array_to_list (argv ()) in
  Miniriot.run ~main ~args ()
