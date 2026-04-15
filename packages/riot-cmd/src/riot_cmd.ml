open Std

module type Command = sig
  val name: string

  val command: ArgParser.command

  val run: args:ArgParser.matches -> (unit, string) result
end

module Registry = struct
  type state = {
    mutable commands: (string * (module Command)) list;
  }

  let registry = { commands = [] }

  let register = fun (cmd: (module Command)) ->
    let module Cmd = (val cmd) in
    Log.debug ("Registering command: " ^ Cmd.name);
    registry.commands <- (Cmd.name, cmd) :: registry.commands

  let get = fun name ->
    List.find_opt (fun ((n, _)) -> n = name) registry.commands
    |> Option.map ~fn:(fun (_, command) -> command)

  let list = fun () -> registry.commands
end
