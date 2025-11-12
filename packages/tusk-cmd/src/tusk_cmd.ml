open Std

module type Command = sig
  val name : string
  val command : ArgParser.command
  val run : args:ArgParser.matches -> (unit, string) result
end

module Registry = struct
  type state = {
    mutable commands : (string * (module Command)) list;
  }
  
  let registry = { commands = [] }
  
  let register (cmd : (module Command)) =
    let module Cmd = (val cmd) in
    Log.debug ("Registering command: " ^ Cmd.name);
    registry.commands <- (Cmd.name, cmd) :: registry.commands
  
  let get name = 
    List.find_opt (fun (n, _) -> n = name) registry.commands
    |> Option.map snd
  
  let list () = registry.commands
end
