(** CLI module - main interface for command-line operations *)

module Build = Build
module Rpc = Rpc

let run args =
  match args with
  | "build" :: rest -> Build.run rest
  | "rpc" :: rest -> Rpc.run rest
  | _ -> Printf.printf "Unknown command\n"