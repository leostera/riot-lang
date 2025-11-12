open Std

let name = "demo"

let command = 
  let open ArgParser in
  let open Arg in
  command name
  |> about "Run a minttea TUI demo"

let main ~args =
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      Error (Failure "Argument parsing failed")
  | Ok _matches -> (
      println "Hello from minttea:demo!";
      println "This is a package-provided Tusk command.";
      println "";
      println "The command system is working correctly!";
      Ok ()
  )

let () = Miniriot.run ~main ~args:Std.Env.args ()
