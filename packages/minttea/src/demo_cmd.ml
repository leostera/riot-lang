open Std

let name = "demo"

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command name
  |> about "Run a minttea TUI demo"

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      Error (Failure "Argument parsing failed")
  | Ok _matches -> (
      println "Hello from minttea:demo!";
      println "This is a package-provided Riot command.";
      println "";
      println "The command system is working correctly!";
      Ok ())

let should_autorun =
  match Std.Env.args with
  | argv0 :: _ -> (
      match Path.from_string argv0 with
      | Ok path -> Path.basename path = name
      | Error _ -> argv0 = name)
  | [] -> false

let () =
  if should_autorun then
    let _ = Runtime.run ~main ~args:Std.Env.args () in
    ()
